module Language.Cobble.Codegen.TestShared where

import Language.Cobble.Prelude
import Language.Cobble.LC.Types as L
import Language.Cobble.CPS.Basic.Types as C
import Language.Cobble.CPS.TopLevel.Types as T

exampleLC :: LCExpr
exampleLC = L.App (L.Lambda "a" (L.Select 1 (L.Var "a"))) (L.Tuple [L.IntLit 3, L.IntLit 4])


exampleCPS :: CPS
exampleCPS = C.App2
                (C.Admin "f0" (C.App2
                    (C.Admin "x0" (C.App2
                        (C.Admin "x1" (C.Let "t2" (C.Tuple [C.Var "x0", C.Var "x1"]) (C.App2
                            (C.Admin "v1" (C.App3 (C.Var "f0") (C.Var "v1") C.Halt))
                            (C.Var "t2")
                            )))
                        (C.IntLit 4)))
                    (C.IntLit 3)))
                (C.Lambda "k3" "a" (C.App2 (Admin "t4" (C.Let "y5" (C.Select 1 (C.Var "t4")) (C.App2 (C.Var "k3") (C.Var "y5")))) (C.Var "a")))

exampleReduced :: CPS
exampleReduced = C.Let "f0" (Val (C.Lambda "k3" "a" (C.Let "y5" (C.Select 1 (C.Var "a")) (C.App2 (C.Var "k3") (C.Var "y5")))))
                    (C.Let "x0" (Val (C.IntLit 3))
                        (C.Let "x1" (Val (C.IntLit 4))
                            (C.Let "t2" (C.Tuple [C.Var "x0", C.Var "x1"])
                                (C.App3 (C.Var "f0") (C.Var "t2") C.Halt))))
exampleReducedFreshIX :: Int
exampleReducedFreshIX = 6

exampleTL :: TL
exampleTL = T.LetF "f6" "k3" ["s7", "a"] (T.Let "y5" (T.Select 1 "a") 
                (T.App "k3" ["y5"]))     
            (T.C 
                (T.Let "env8" (T.Tuple [])
                (T.Let "f0" (T.Tuple ["f6", "env8"])
                (T.Let "x0" (T.IntLit 3)
                (T.Let "x1" (T.IntLit 4)
                (T.Let "t2" (T.Tuple ["x0", "x1"])
                (T.Let "e9" T.Halt
                (T.Let "f10" (T.Select 0 "f0")
                (T.Let "env11" (T.Select 1 "f0")
                (T.App "f10" ["env11", "t2", "e9"])
                )))))))))
