{-# LANGUAGE NoImplicitPrelude, DataKinds, ConstraintKinds, PostfixOperators, TypeApplications #-}
{-# LANGUAGE LambdaCase, MultiWayIf, FlexibleContexts, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
module Language.Cobble.Parser.Tokenizer where

import Language.Cobble.Prelude

import Language.Cobble.Types

import Data.Char

import Text.Read (read)

-- | A Token, indexed by a parameter of kind `Processing`, 
-- consists of its lexical information
-- as well as the actual token data.
-- See the haddock for `Processing` and `TokenData` for
-- information about those.
data Token (p :: Processing) = Token {
      tokLexInfo::LexInfo
    , tokData :: TokenData p
    }  deriving (Show, Eq)

-- | A data kind representing the processing
-- status of a token. This is (currently) only used
-- as a phantom parameter, so it should be very easy to convert
-- if you really have to.
-- Here 'processing status' refers to, whether Macro Names in `Ident`s have been 
-- replaced by `MacroCall` Tokens.
data Processing = Unprocessed
                | Processed


data TokenData (p :: Processing) = Ident Text
         | Reserved Text
         | Paren Text
         | Operator Text
         | ReservedOp Text
         | IntLiteral Int
         | MacroCall Text
         deriving (Show, Eq)

isOpStart :: Char -> Bool
isOpStart = isOpLetter

isOpLetter :: Char -> Bool
isOpLetter = flip elem "+-*/~^!?.|<>$&=#:;,"

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c

isIdentLetter :: Char -> Bool
isIdentLetter c = isAlphaNum c || c `elem` "_"

reserved :: [String]
reserved = ["True", "False", "defmacro", "void", "let", "while", "int", "bool"]

reservedOps :: [String]
reservedOps = [":", ";", ",", "=", "=>"]

isParen :: Char -> Bool
isParen = (`elem`"()[]{}")

data LexicalError = LexicalError LexInfo LexicalErrorData deriving (Show, Eq)

data LexicalErrorData = ReachedEOF
                  | UnexpectedChar Char
                  | UnexpectedCharInIdent Char
                  | UnexpectedCharInOp Char
                  | UnexpectedCharInIntLit Char
                  deriving (Show, Eq)

data TokenState = Default
                | InIdent [Char]
                | InOp [Char]
                | InIntLit [Char]
                deriving (Show, Eq)

type TokenizeC r = Members [State LexInfo, State (TokenState, LexInfo), Error LexicalError] r

askChar :: (TokenizeC r, Member (State [Char]) r) => Sem r (Maybe Char)
askChar = do
    get >>= \case
        [] -> pure Nothing
        (c:cs) -> do
            put cs
            if (c `isNewline`) then
                modify (\ls -> ls{line=line ls + 1, column=0})
            else
                modify (\ts -> ts{column=column ts + 1})
            pure $ Just c

putS :: (TokenizeC r) => TokenState -> Sem r ()
putS ts = do
    li <- gets snd
    put (ts, li)

putStart :: (TokenizeC r) => TokenState -> Sem r ()
putStart ts = do
    li <- get
    put (ts, li)

throwL :: (TokenizeC r) => LexicalErrorData -> Sem r ()
throwL d = get >>= \li -> throw (LexicalError li d)

tellToken :: (TokenizeC r, Member (Writer [Token 'Unprocessed]) r) => TokenData 'Unprocessed -> Sem r ()
tellToken td = do
    lexInfo <- gets snd
    tell [Token lexInfo td]

tellTokenNewLex:: (TokenizeC r, Member (Writer [Token 'Unprocessed]) r) => TokenData 'Unprocessed -> Sem r ()
tellTokenNewLex td = do
    lexInfo <- get
    tell [Token lexInfo td]


tokenize :: FileName -> Text -> Either LexicalError [Token 'Unprocessed]
tokenize fileName text = run $ runError $ evalState initialLex $ evalState (Default, initialLex) $ tokenize' (toString text)
    where
        initialLex = LexInfo 1 0 fileName

tokenize' :: TokenizeC r => [Char] -> Sem r [Token 'Unprocessed]
tokenize' input = fmap fst $ runWriterAssocR $ evalState input $ go
    where
        go :: (TokenizeC r, Members [Writer [Token 'Unprocessed], State [Char]] r) => Sem r ()
        go = gets fst >>= \case
            Default -> askChar >>= \case
                Nothing -> pass
                Just c -> if
                    | isIdentStart c -> putStart (InIdent [c]) >> go
                    | isOpStart c -> putStart (InOp [c]) >> go
                    | isDigit c -> putStart (InIntLit [c]) >> go
                    | isWhiteSpace c -> go
                    | isParen c -> tellTokenNewLex (Paren (one c)) >> putStart Default >> go
                    | otherwise -> throwL $ UnexpectedChar c
            InIdent cs -> askChar >>= \case
                Nothing -> endIdent cs
                Just c -> if
                    | isIdentLetter c -> putS (InIdent (cs <> [c])) >> go
                    | isWhiteSpace c -> endIdent (cs) >> putStart Default >> go
                    | isOpStart c -> endIdent cs >> putStart (InOp [c]) >> go
                    | isParen c -> endIdent cs >> tellTokenNewLex (Paren (one c)) >> putStart Default >> go
                    | otherwise -> throwL $ UnexpectedCharInIdent c
                -- TODO
                where
                    endIdent cs
                        | cs `elem` reserved = tellToken (Reserved (toText cs))
                        | otherwise = tellToken (Ident (toText cs))
            InOp cs -> askChar >>= \case
                Nothing -> endOp cs
                Just c -> if
                    | isOpLetter c -> putS (InOp (cs <> [c])) >> go
                    | isDigit c -> endOp cs >> putStart (InIntLit [c]) >> go
                    | isIdentStart c -> endOp cs >> putStart (InIdent [c]) >> go
                    | isWhiteSpace c -> endOp cs >> putStart Default >> go
                    | isParen c -> endOp cs >> tellTokenNewLex (Paren (one c)) >> putStart Default >> go
                    | otherwise -> throwL $ UnexpectedCharInOp c
                where
                    endOp cs
                        | cs `elem` reservedOps = tellToken (ReservedOp (toText cs))
                        | otherwise = tellToken (Operator (toText cs))
            InIntLit cs -> askChar >>= \case
                Nothing -> endIntLit cs
                Just c -> if
                    | isDigit c -> putS (InIntLit (cs <> [c])) >> go
                    | isWhiteSpace c -> endIntLit cs >> putS Default >> go
                    | isOpStart c -> endIntLit cs >> putS (InOp [c]) >> go
                    | isParen c -> endIntLit cs >> tellTokenNewLex (Paren (one c)) >> putStart Default >> go
                    | otherwise -> throwL $ UnexpectedCharInIntLit c
                where
                    endIntLit = tellToken . IntLiteral . read

isNewline :: Char -> Bool
isNewline = (=='\n')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"