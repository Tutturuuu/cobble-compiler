{-# LANGUAGE TemplateHaskell #-}
module Language.Cobble.Qualifier where

import Language.Cobble.Prelude
import Language.Cobble.Types
import Language.Cobble.Types.Lens

import Language.Cobble.Util
import Language.Cobble.Util.Bitraversable
import Language.Cobble.Util.Polysemy.Fresh

import Data.Map qualified as M

type NextPass = SemAnalysis
  
--type QualifyC r = Members '[State Int, State [Scope], Error QualificationError, Output Log] r

data QualificationError = NameNotFound LexInfo Text
                        | TypeNotFound LexInfo Text
                        | NotAStruct LexInfo QualifiedName Kind TypeVariant
                        | VarAlreadyDeclaredInScope LexInfo Text
                        | TypeAlreadyDeclaredInScope LexInfo Text
                        | AmbiguousVarName LexInfo Text [QualifiedName]
                        | AmbiguousTypeName LexInfo Text [(QualifiedName, Kind, TypeVariant)]
                        deriving (Show, Eq)

data Scope = Scope {
        _scopeVars :: Map Text QualifiedName
    ,   _scopeTypes :: Map Text (QualifiedName, Kind, TypeVariant)
    } deriving (Show, Eq)

makeLenses ''Scope

lookupVar :: Members '[Reader [Scope], Error QualificationError] r => LexInfo -> Text -> Sem r QualifiedName 
lookupVar l n = do
    scopes <- ask
    case mapMaybe (lookup n . view scopeVars) scopes of 
        []  -> throw $ NameNotFound l n
        [x] -> pure x
        xs  -> throw $ AmbiguousVarName l n xs

lookupType :: Members '[Reader [Scope], Error QualificationError] r => LexInfo -> Text -> Sem r (QualifiedName, Kind, TypeVariant) 
lookupType l n = do
    scopes <- ask
    case mapMaybe (lookup n . view scopeTypes) scopes of 
        []  -> throw $ TypeNotFound l n
        [x] -> pure x
        xs  -> throw $ AmbiguousTypeName l n xs


withVar :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
        => LexInfo 
        -> Text 
        -> (QualifiedName -> Sem r a) 
        -> Sem r a
withVar l n a = do
                n' <- freshVar (n, l)
                alreadyInScope <- asks (member n .view (_head . scopeVars))
                if alreadyInScope 
                then throw (VarAlreadyDeclaredInScope l n)
                else local (_head . scopeVars %~ insert n n') (a n')

withType :: Members '[Reader [Scope], Fresh (Text, LexInfo) QualifiedName, Error QualificationError] r 
         => LexInfo -> Text 
         -> Kind
         -> TypeVariant
         -> (QualifiedName -> Sem r a) 
         -> Sem r a
withType l n k tv a = do
                n' <- freshVar (n, l)
                alreadyInScope <- asks (member n . view (_head . scopeTypes))
                if alreadyInScope 
                then throw (TypeAlreadyDeclaredInScope l n)
                else local (_head . scopeTypes %~ insert n (n', k, tv)) (a n')

withDeps :: (Members '[Reader [Scope]] r) => Dependencies -> Sem r a -> Sem r a
withDeps deps a = foldr (\x r -> local (x:) r) a $ map (modSigToScope . snd) $ M.toList deps
    where
        modSigToScope (ModSig {exportedVars, exportedTypes}) = Scope {
                _scopeVars  = fromList $ map (\(n,_) -> (originalName n, n)) $ M.toList exportedVars
            ,   _scopeTypes = fromList $ map (\(n,(k,v)) -> (originalName n, (n, k, v))) $ M.toList exportedTypes
            }

qualify :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
        => Module QualifyNames
        -> Sem r (Module NextPass)
qualify (Module (Ext deps) name stmnts) = withDeps deps 
                                        $ Module (Ext deps) (UnsafeQualifiedName name name (LexInfo (SourcePos 0 0) (SourcePos 0 0) name)) 
                                       <$>qualifyStmnts stmnts

qualifyStmnts :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
             => [Statement QualifyNames]
             -> Sem r [Statement NextPass]
qualifyStmnts = \case
    [] -> pure []
    (Def IgnoreExt li decl@(Decl _ n _ _) ty : sts) -> withVar li n $ \n' -> 
        (:)
        <$> (Def IgnoreExt li
                <$> qualifyDeclWith n' li decl
                <*> qualifyType li ty)
        <*> qualifyStmnts sts

    (Import IgnoreExt li m : sts) -> 
        (:)
        <$> pure (Import IgnoreExt li $ UnsafeQualifiedName m m li)
        <*> qualifyStmnts sts

-- Once parametric types are a thing, the kind will have to be determined by the type parameters (and the way they are used).
-- for now, the only kind to put here is KStar
    (DefStruct IgnoreExt li n fields : sts) -> traverse (bitraverse pure (qualifyType li)) fields >>= \fields' ->
        withType li n KStar (RecordType (map (second coercePass) fields')) $ \qn ->
            (:) 
                <$> pure (DefStruct IgnoreExt li qn fields')
                <*> qualifyStmnts sts

    (StatementX x _ : _) -> absurd x

qualifyExp :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
           => Expr QualifyNames
           -> Sem r (Expr NextPass)
qualifyExp = \case
    FCall IgnoreExt li e args   -> FCall IgnoreExt li <$> qualifyExp e <*> traverse qualifyExp args
    IntLit IgnoreExt li n       -> pure $ IntLit IgnoreExt li n
    UnitLit li                  -> pure $ UnitLit li
    If IgnoreExt li cond th el  -> If IgnoreExt li <$> qualifyExp cond <*> qualifyExp th <*> qualifyExp el
    Let IgnoreExt li decl@(Decl _ n _ _) b  -> withVar li n $ \n' -> Let IgnoreExt li <$> qualifyDeclWith n' li decl <*> qualifyExp b
    Var IgnoreExt li n          -> Var IgnoreExt li <$> lookupVar li n
    StructConstruct IgnoreExt li structName fields -> lookupType li structName >>= \case
            (structName', _k, RecordType tyFields) -> traverse (secondM qualifyExp) fields <&> \fields' -> 
                StructConstruct (StructDef structName' (map (second coercePass) tyFields)) li structName' fields'
            (tyName', k, variant) -> throw (NotAStruct li tyName' k variant)
    StructAccess IgnoreExt li se f -> do
        allStructs :: [(QualifiedName, Kind, TypeVariant)] <- asks (concatMap (toList . view scopeTypes))
        let possibleStructs = fromList $ allStructs & mapMaybe \(tyName, kind, tyVariant) -> case tyVariant of
                RecordType fields -> if f `elem` (map fst fields) 
                    then Just $ (tyName, StructDef tyName (map (second coercePass) fields))
                    else Nothing 
                _ -> Nothing
        StructAccess possibleStructs li 
            <$> qualifyExp se
            <*> pure f
    ExprX x _ -> absurd x

qualifyDeclWith :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
                => QualifiedName
                -> LexInfo
                -> Decl QualifyNames
                -> Sem r (Decl NextPass)
qualifyDeclWith n' li (Decl IgnoreExt _ (Ext params) e) = 
    uncurry (Decl IgnoreExt n' . Ext)
    <$> foldr (\p r -> withVar li p \p' -> first (p' :) <$> r) (([],) <$> qualifyExp e) params

qualifyDecl :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
            => LexInfo
            -> Decl QualifyNames
            -> Sem r (Decl NextPass)
qualifyDecl li d@(Decl _ n _ _) = withVar li n \n' -> qualifyDeclWith n' li d

qualifyType :: Members '[Error QualificationError, Reader [Scope], Fresh (Text, LexInfo) QualifiedName] r
            => LexInfo 
            -> Type QualifyNames
            -> Sem r (Type NextPass)
qualifyType li = \case 
    TCon n ()           -> (\(n', k, _) -> TCon n' k) <$> lookupType li n
    TVar (MkTVar n ())  -> pure $ TVar (MkTVar (UnsafeQualifiedName n n li) KStar)  -- TODO
    TApp t1 t2          -> TApp <$> qualifyType li t1 <*> qualifyType li t2 
