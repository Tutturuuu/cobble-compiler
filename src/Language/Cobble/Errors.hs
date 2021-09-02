module Language.Cobble.Errors where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble

import Language.Cobble.Parser.Tokenizer (LexicalError(..))
import Language.Cobble.Prelude.Parser (ParseError)
import Language.Cobble.ModuleSolver (ModuleError (..))
import Language.Cobble.SemAnalysis (SemanticError (..))
import Language.Cobble.Qualifier (QualificationError (..))
import Language.Cobble.Typechecker (TypeError (..))
import Errata
import Errata.Source

prettyPrintError :: CompilationError -> IO ()
prettyPrintError err = readFile (sourcePath err) >>= \source -> putLTextLn $ prettyErrors source $ pure $ case err of
    LexError e -> prettyPrintLexError e
    ParseError e -> prettyPrintParseError e
    QualificationError e -> prettyPrintQualError e
    SemanticError e -> prettyPrintSemError e
    TypeError e -> prettyPrintTypeError e
    ModuleError e -> prettyPrintModError e
    Panic e -> prettyPrintPanic e

prettyPrintLexError :: LexicalError -> Errata
prettyPrintLexError = tempError 

prettyPrintParseError :: ParseError -> Errata
prettyPrintParseError = tempError

prettyPrintQualError :: QualificationError -> Errata
prettyPrintQualError = \case
    NameNotFound li name    -> commonError li ["Variable not in scope: ", red name] ["You might have misspelled something or forgotten an import"]
    TypeNotFound li name    -> commonError li ["Type not in scope: ", red name]     ["You might have misspelled something or forgotten an import"]
    FixityNotFound li name  -> commonError li ["No declared fixity for ", customNameT name] ["Your probably forgot a fixity annotation in the declaration of ", customNameT name]
    NotAStruct li tname _tkind _tvar -> commonError li ["The type ", customName tname, " is not a struct type"] ["You can only assign fields on a struct type"]
    VarAlreadyDeclaredInScope li var -> commonError li ["The variable ", customNameT var, " has already been declared in the current scope"] ["Name shadowing in the same scope is not permitted in cobble."]
    TypeAlreadyDeclaredInScope li ty -> commonError li ["The type ", customNameT ty, " has already been declared in the current scope"] []
    e -> tempError e 

prettyPrintSemError :: SemanticError -> Errata
prettyPrintSemError = tempError 

prettyPrintTypeError :: TypeError -> Errata
prettyPrintTypeError = tempError

prettyPrintModError :: ModuleError -> Errata
prettyPrintModError = tempError

prettyPrintPanic :: Text -> Errata
prettyPrintPanic = tempError

customNameT :: Text -> Format
customNameT = Bold . pure . quoted 

customName :: QualifiedName -> Format
customName = customNameT . originalName

data Format = Text Text | Red [Format] | Bold [Format] | Quoted [Format]

red :: Text -> Format
red = Red . pure . Text

bold :: Text -> Format
bold = Bold . pure . Text

quoted :: Text -> Format
quoted = Quoted . pure . Text

renderFormat :: [Format] -> Text
renderFormat = foldMap (\x -> renderFormat' x <> "\ESC[0m")

renderFormat' :: Format -> Text
renderFormat' = \case
    Text t  -> t
    Red fs  -> "\ESC[31m" <> foldMap renderFormat' fs
    Bold fs -> "\ESC[1m" <> foldMap renderFormat' fs
    Quoted fs -> "'" <> foldMap renderFormat' fs <> "'"

instance IsString Format where
    fromString = Text . toText

commonError :: LexInfo -> [Format] -> [Format] -> Errata
commonError (LexInfo from to fp) name desc = errataSimple 
    (Just $ renderFormat name) (blockSimple 
                fancyRedStyle 
                (toString fp) 
                Nothing  
                (line from, column from, column to, Nothing) 
                Nothing)  
            (Just (renderFormat desc))

tempError :: (Show s) => s -> Errata
tempError s = Errata (Just (show s)) [] Nothing

class HasSourcePath e where
    sourcePath :: e -> FilePath

instance HasSourcePath CompilationError where
    sourcePath (QualificationError e) = sourcePath e
    sourcePath e = error $ "sourcePath not complete: " <> show e

instance HasSourcePath QualificationError where
    sourcePath (NameNotFound (LexInfo _ _ fp) _) = toString fp
    sourcePath (TypeNotFound (LexInfo _ _ fp) _) = toString fp
    sourcePath (FixityNotFound (LexInfo _ _ fp) _) = toString fp
    sourcePath (NotAStruct (LexInfo _ _ fp) _ _ _) = toString fp
    sourcePath (VarAlreadyDeclaredInScope (LexInfo _ _ fp) _) = toString fp
    sourcePath (TypeAlreadyDeclaredInScope (LexInfo _ _ fp) _) = toString fp
    sourcePath e = error $ "sourcePath not complete: " <> show e


