{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module Language.Cobble.Types.AST.PostProcess where
  
import Data.Data
import Data.Generics.Uniplate.Data
  
import Language.Cobble.Prelude

import Language.Cobble.Types.AST
import Language.Cobble.Types.Instances
import Language.Cobble.Types.QualifiedName

type instance XModule PostProcess = Ext PostProcess (Map (Name PostProcess) ModSig)

type instance XDecl      PostProcess = Ext PostProcess (Type PostProcess)
type instance XParam     PostProcess = Ext PostProcess [(Name PostProcess, Type PostProcess)]

type instance XDef       PostProcess = IgnoreExt PostProcess
type instance XImport    PostProcess = IgnoreExt PostProcess
type instance XDefStruct PostProcess = Ext PostProcess Kind
type instance XStatement PostProcess = ExtVoid PostProcess

type instance XFCall            PostProcess = Ext PostProcess (Type PostProcess)
type instance XIntLit           PostProcess = IgnoreExt PostProcess
type instance XIf               PostProcess = IgnoreExt PostProcess
type instance XLet              PostProcess = IgnoreExt PostProcess
type instance XVar              PostProcess = Ext PostProcess (Type PostProcess)
type instance XStructConstruct  PostProcess = Ext PostProcess (StructDef PostProcess, Type PostProcess)
type instance XStructAccess     PostProcess = Ext PostProcess (Map QualifiedName (StructDef PostProcess), Type PostProcess, Type PostProcess)
type instance XExpr             PostProcess = ExtVoid PostProcess

type instance Name PostProcess = QualifiedName

type instance XKind PostProcess = Kind
