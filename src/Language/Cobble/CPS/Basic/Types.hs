module Language.Cobble.CPS.Basic.Types where

import Language.Cobble.Prelude 
import Language.Cobble.Shared
import Language.Cobble.Codegen.Common

import qualified GHC.Show as S
import qualified Data.Text as T

import Data.Data

data CPS = Let QualifiedName CPSExpr CPS
         | App3 CPSVal CPSVal CPSVal
         | App2 CPSVal CPSVal
         deriving (Eq, Generic, Data)
instance S.Show CPS where show = toString . prettyPrintCPS

data CPSExpr = Val CPSVal
             | Tuple [CPSVal]
             | Select Int CPSVal
             deriving (Eq, Generic, Data)
instance S.Show CPSExpr where show = toString . prettyPrintCPSExpr

data CPSVal = IntLit Int
            | Var QualifiedName
            | Lambda QualifiedName QualifiedName CPS
            | Admin QualifiedName CPS
            | Halt
            deriving (Eq, Generic, Data)
instance S.Show CPSVal where show = toString . prettyPrintCPSVal


prettyPrintCPS :: CPS -> Text
prettyPrintCPS = \case
    Let name ex body    -> "let " <> show name <> " = " <> prettyPrintCPSExpr ex <> " in " <> prettyPrintCPS body
    App2 f x            -> prettyPrintCPSVal f <> " " <> prettyPrintCPSVal x
    App3 f x y          -> prettyPrintCPSVal f <> " " <> prettyPrintCPSVal x <> " " <> prettyPrintCPSVal y

prettyPrintCPSExpr :: CPSExpr -> Text
prettyPrintCPSExpr = \case
    Val v       -> prettyPrintCPSVal v
    Tuple vs    -> "(" <> T.intercalate ", " (map prettyPrintCPSVal vs) <> ")"
    Select i v  -> "#" <> show i <> " " <> prettyPrintCPSVal v

prettyPrintCPSVal :: CPSVal -> Text
prettyPrintCPSVal = \case
    IntLit i        -> show i
    Var n           -> show n
    Halt            -> "halt"
    Lambda k x b    -> "(λ" <> show k <> " " <> show x <> ". " <> prettyPrintCPS b <> ")"
    Admin x b       -> "(λ_" <> show x <> ". " <> prettyPrintCPS b <> ")"


