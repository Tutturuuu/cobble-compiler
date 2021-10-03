module Language.Cobble.Codegen.LCToBasicCPS where

import Language.Cobble.Prelude
import Language.Cobble.Util
import Language.Cobble.Util.Polysemy.Fresh
import Language.Cobble.Types.QualifiedName
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.Codegen.Common

import Data.List ((!!))

compile :: (Members '[Fresh Text QualifiedName] r) => LCExpr -> Sem r CPS
compile e = compileExpr e Halt Halt

compileExpr :: (Members '[Fresh Text QualifiedName] r) => LCExpr -> CPSVal -> CPSVal -> Sem r CPS
compileExpr e kval krval = let k  = staticCont kval 
                               kr = staticCont krval
                           in case e of
    L.Var v             -> pure $ k (C.Var v)
    L.IntLit n          -> pure $ k (C.IntLit n)
    L.Lambda x e        -> freshVar2 "k" "kr" >>= \(k', kr') -> k . C.Lambda k' kr' x <$> compileExpr e (C.Var k') (C.Var kr')
    L.Let v e b         -> do 
        k' <- C.Admin v <$> compileExpr b kval krval
        compileExpr e k' krval
    L.LetRec f x e b    -> freshVar2 "k" "kr" >>= \(k', kr') -> C.LetRec f k' kr' x 
        <$> compileExpr e (C.Var k') (C.Var kr')
        <*> compileExpr b kval krval
    L.App f a           -> freshVar2 "f" "v" >>=  \(f', v') -> do
        k' <- C.Admin f' <$> compileExpr a (C.Admin v' (C.App4 (C.Var f') kval krval (C.Var v'))) krval
        compileExpr f k' krval
    L.Tuple es          -> freshVar "t" >>= \t' -> do
        vars <- traverse freshVar $ map (("x" <>) . show) (indexes es)
        foldrM (\(i, e) r -> compileExpr e (C.Admin (vars !! i) r) krval)
            (C.Let t' (C.Tuple (map C.Var vars)) (k (C.Var t')))
            (zipIndex es)
    L.Select n e        -> freshVar "t" >>= \t' -> freshVar "y" >>= \y' -> compileExpr e (C.Admin t' (C.Let y' (C.Select n (C.Var t')) (k (C.Var y')))) krval
    L.PrimOp p es       -> freshVar "p" >>= \p' -> do
        vars <- traverse freshVar $ map (("x" <>) . show @Text) (indexes es)
        foldrM (\(i, e) r -> compileExpr e (C.Admin (vars !! i) r) krval)
            (C.Let p' (C.PrimOp p (map C.Var vars)) (k (C.Var p')))
            (zipIndex es)
    --                            We bind kval as k' so the (potentially large) continuation is not (yet) duplicated.
    L.If c th el -> freshVar2 "c" "k" >>= \(c', k') -> do
        k'' <- (.) (C.Admin c') . C.If (C.Var c') 
                    <$> compileExpr th (C.Var k') krval
                    <*> compileExpr el (C.Var k') krval
        C.Let k' (Val kval) <$> compileExpr c k'' krval 
    -- Here we bind kval to k' because that is what @e@ expects, *not* just to prevent duplication
    L.Shift k' e -> C.Let k' (C.Val kval) <$> compileExpr e krval krval
    -- We bind kval to k' so the continuation is not duplicated
    L.Reset e    -> freshVar "k" >>= \k' -> C.Let k' (C.Val kval) <$> compileExpr e (C.Var k') (C.Var k') 


reduceAdmin :: CPS -> CPS
reduceAdmin = rewriteBi (betaLetReduce <<|>> etaReduce)
    where
        betaLetReduce :: CPS -> Maybe CPS
        betaLetReduce = \case
--          (\_x. e) y
            C.App2 (C.Admin x e) y
                | C.Var _ <- y                                      -> Just $ replaceVar x y e
                --  | length [() | C.Var v <- universeBi e, v == x] <= 1 -> Just $ replaceVar x y e
                -- TODO^: Should inlining be done in a different step?
                -- TODO: This might change evaluation order but in a purely functional language that is not important
                -- TODO: (IO might need some special cases here)
                | otherwise                                          -> Just $ C.Let x (C.Val y) e
            _                                                        -> Nothing

        etaReduce :: CPS -> Maybe CPS
        etaReduce c = case [() | C.Admin x (C.App2 _k (C.Var x')) <- universeBi c, x == x'] of
            [] -> Nothing
            _  -> Just $ c & transformBi \case
                C.Admin x (C.App2 k (C.Var x')) | x == x' -> k
                x                                         -> x

        replaceVar :: QualifiedName -> CPSVal -> CPS -> CPS
        replaceVar v e = transformBi \case
            (C.Var v') | v == v' -> e
            x -> x
                        
staticCont :: CPSVal -> (CPSVal -> CPS)
staticCont = C.App2
