module Language.Cobble.TypecheckerSpec where

import Language.Cobble.Prelude
import Language.Cobble.Typechecker as TC hiding (Type)
import Language.Cobble.Types



import Language.Cobble.Parser qualified as C
import Language.Cobble.Parser.Tokenizer qualified as C
import Language.Cobble.Qualifier qualified as C
import Language.Cobble.SemAnalysis qualified as C
import Language.Cobble.Prelude.Parser qualified as C (ParseError, parse)
import Language.Cobble.Util.Polysemy.Fresh qualified as C
import Language.Cobble.Util.Polysemy.Dump qualified as C
import Language.Cobble qualified as C

import Test.Hspec as S
import GHC.Base (errorWithoutStackTrace)

spec :: Spec
spec = do
    it "really simple variable inference" do
        runTypecheck [
                "x :: Int;"
            ,   "x = let y = 5 in let z = y in y;"
            ] `shouldSatisfy` \ast -> case [ty | Var (Ext ty) _ (ReallyUnsafeQualifiedName "y" _ _) :: Expr PostProcess <- universeBi ast] of
                    [] -> False
                    xs -> all (== intT) xs

    it "no let generalization" do
        runTypecheck [
                "x :: Int;"
            ,   "x = let f y = y in f 3;"
            ] `shouldSatisfy` \ast -> case [ty | VarType ty "f" <- universeBi ast ] of
                [TCon (QName "Int") KStar :-> TCon (QName "Int") KStar] -> True
                tys -> errorWithoutStackTrace $ toString $ ppTypes tys
    
    it "no let generalization [2]" do 
        runTypecheck [
                "g :: Unit -> Int;"
            ,   "g x = 3;"

            -- workaround, since the test setup doesn't know about primops
            ,   "h :: Int -> Int -> Int;"
            ,   "h x y = x;"

            ,   "x :: Int;"
            ,   "x = let f y = y in h (f 5) (g (f ()));"
            ] `shouldSatisfy` \case
                Left (DifferentConstructor _ (TCon (QName "Int") KStar) (TCon (QName "Unit") KStar)) -> True
                e -> errorWithoutStackTrace $ show e 

    it "top level statements keep polymorphic type signature" do
        runTypecheck [
                "f :: a -> a;"
            ,   "f x = x;"

            ,   "y :: Int;"
            ,   "y = f 5;"

            ,   "z :: Unit;"
            ,   "z = f ();"
            ] `shouldSatisfy` \ast -> case [ty | Def _ _ (Decl _ (QName "f") _ _) ty <- universeBi ast] of
                [TForall [MkTVar (QName "a") KStar] (TVar (MkTVar (QName "a") KStar) :-> TVar (MkTVar (QName "a") KStar))] -> True
                tys -> errorWithoutStackTrace $ toString $ ppTypes tys
    -- Not sure if this *really* works... seemed a bit too easy
    it "tyvars in top level bindings become skolems" do
        runTypecheck [
                "f :: a -> a;"
            ,   "f x = 5;"
            ] `shouldSatisfy` \case
                Left (SkolBinding _ (TSkol (MkTVar (QName "a") KStar)) (TCon (QName "Int") KStar)) -> True
                e -> errorWithoutStackTrace $ show e

    it "different skolems don't unify" do
        runTypecheck [
                "const :: a -> b -> a;"
            ,   "const x y = y;"
            ] `shouldSatisfy` \case
                Left (SkolBinding _ (TSkol (MkTVar (QName "a") KStar)) (TSkol (MkTVar (QName "b") KStar))) -> True
                e -> errorWithoutStackTrace $ show e

    it "top level (forall) types keep foralls on local variable AST and are not instantiated (is this correct?)" do
        runTypecheck [
                "f :: a -> a;"
            ,   "f x = x;"

            ,   "g :: Int;"
            ,   "g = f 5;"
            ] `shouldSatisfy` \ast -> case [ty | VarType ty "f" <- universeBi ast] of
                [TForall [MkTVar (QName "a") KStar] (TVar (MkTVar (QName "a") KStar) :-> TVar (MkTVar (QName "a") KStar))] -> True
                tys -> errorWithoutStackTrace $ toString $ ppTypes tys

    it "type variables and skolems unify" do
        runTypecheck [
                "f :: a -> a;"
            ,   "f x = x;"

            ,   "const :: a -> b -> a;"
            ,   "const x y = f x;"
            ] `shouldSatisfy` \ast -> case [ty | FCall (Ext ty) _ (VarType _ "f") _ <- universeBi ast] of
                [TSkol (MkTVar (QName "a") KStar)] -> True
                ts -> errorWithoutStackTrace $ toString $ ppTypes ts

ppTypes :: [Type PostProcess] -> Text
ppTypes = unlines . map ppType

pattern VarType :: Type PostProcess -> Text -> Expr PostProcess
pattern VarType ty name <- Var (Ext ty) _ (ReallyUnsafeQualifiedName name _ _)

pattern QName :: Text -> QualifiedName
pattern QName name <- ReallyUnsafeQualifiedName name _ _

runTypecheck :: [Text] -> Either TypeError (Module PostProcess)
runTypecheck mod = run $ evalState (TCState mempty) $ ignoreOutput @Log $ C.dontDump @[TConstraint] $ runError $ C.runFreshQNamesState $ C.freshWithInternal 
                       $ cobbleCode (unlines mod) >>= typecheck

cobbleCode :: (Member (C.Fresh (Text, LexInfo) QualifiedName) r) => Text -> Sem r (Module Typecheck)
cobbleCode content = do
    toks <- either (\(e :: C.LexicalError) -> error $ "lex error: " <> show e) id <$> runError (C.tokenize "test.cb" content)
    let parsed = either (\e -> error $ "parse error: " <> show e) id $ C.parse (C.module_ "test.cb") "" toks
    
    let moduleSolved :: Module QualifyNames = let (Module IgnoreExt pmname psts) = parsed 
            in 
            Module (Ext (one (internalQName "prims", C.primModSig))) pmname (coercePass psts) 

    let qualScopes = [C.Scope {
            _scopeVars = mempty
        ,   _scopeTypes = mempty
        ,   _scopeFixities = mempty
        ,   _scopeTVars = mempty
        }]
    qualified <- fmap (either (\e -> error $ "qualification error:" <> show e) id) $ runError @C.QualificationError $ runReader qualScopes $ C.qualify moduleSolved
    fmap (either (\e -> error $ "semantic error: " <> show e) id) $ runError @C.SemanticError $ C.runSemanticAnalysis qualified 


