{-#LANGUAGE DataKinds, TemplateHaskell#-}
{-# LANGUAGE PatternSynonyms, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
module Language.Cobble.Types.AST.Typecheck where

import Language.Cobble.Types.AST
import Language.Cobble.Types.TH
  
deriving instance Show (Statement 'Typecheck)
deriving instance Eq (Statement 'Typecheck)

deriving instance Show (Expr 'Typecheck)
deriving instance Eq (Expr 'Typecheck)
  
type instance XCallFun 'Typecheck = ()
type instance XDefVoid 'Typecheck = ()
type instance XDefFun 'Typecheck = ()
type instance XDecl 'Typecheck = ()
type instance XAssign 'Typecheck = ()
type instance XWhile 'Typecheck = ()
type instance XDefStruct 'Typecheck = ()
type instance XStatement 'Typecheck = ()

type instance XFCall 'Typecheck = ()
type instance XIntLit 'Typecheck = ()
type instance XBoolLit 'Typecheck = ()
type instance XVar 'Typecheck = ()
type instance XExpr 'Typecheck = ()

type instance TypeInfo 'Typecheck = Type

makeSynonyms 'Typecheck ''Statement "U"

makeSynonyms 'Typecheck ''Expr "U"

