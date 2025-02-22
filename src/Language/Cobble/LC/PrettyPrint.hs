module Language.Cobble.LC.PrettyPrint where

import Language.Cobble.Prelude
import Language.Cobble.LC.Types

import qualified Data.Text as T

prettyPrintLCDefs :: [LCDef] -> Text
prettyPrintLCDefs = T.intercalate "\n\n" . map prettyPrintLCDef

prettyPrintLCDef :: LCDef -> Text
prettyPrintLCDef (LCDef name expr) = show name <> " = " <> prettyPrintLCExpr expr

prettyPrintLCExpr :: LCExpr -> Text
prettyPrintLCExpr = \case
    Var name                -> show name
    Lambda vname expr       -> "λ" <> show vname <> ". " <> prettyPrintLCExpr expr
    Let vname expr body     -> "let "    <> show vname <> " = " <> prettyPrintLCExpr expr <> " in " <> prettyPrintLCExpr body
    LetRec f x expr body    -> "letrec " <> show f <> " " <> show x <>  " = " <> prettyPrintLCExpr expr <> " in " <> prettyPrintLCExpr body 
    App fexpr aexpr         -> prettyPrintLCExprParens fexpr <> " " <> prettyPrintLCExprParens aexpr
    IntLit i                -> show i
    Tuple as                -> "[" <> T.intercalate ", " (map prettyPrintLCExpr $ toList as) <> "]"
    Select i t              -> prettyPrintLCExprParens t <> "._" <> show i
    If c th el              ->      "if " <> prettyPrintLCExpr c 
                                <> " then " <> prettyPrintLCExpr th 
                                <> " else " <> prettyPrintLCExpr el
    PrimOp p ps             -> "__" <> show p <> "__[" <> T.intercalate ", " (map prettyPrintLCExpr ps) <> "]"

prettyPrintLCExprParens :: LCExpr -> Text
prettyPrintLCExprParens = \case
    Var name -> prettyPrintLCExpr (Var name)
    Tuple as -> prettyPrintLCExpr (Tuple as)
    IntLit i -> prettyPrintLCExpr (IntLit i)
    x -> "(" <> prettyPrintLCExpr x <> ")"


