module Lambda.SCSpec where

import Test.Hspec 
import qualified Lambda.SCCompiler as SC
import qualified Lambda.Syntax as S
import qualified Miranda.Syntax as M

import qualified Transform as Trans
import qualified Compile as Comp

import SpecUtil

{-
App (App (NComb "$1" ["x"] (App (NComb "$2" ["x","y"] (App (App (Term -) (Term "y")) 
                                                           (Term "x"))) 
                                (Term "x"))) 
         (Term 3)) 
    (Term 4)
-}

spec :: Spec 
spec = do 
  xdescribe "Super Combinator Compilation" $ do 
    it "p225: compiles simple expression" $ do
      -- > (\x. (\y. + y x))
      let lam = S.mkApply [
              S.mkLambda ["x", "y"] (S.mkApply [ S.mkFunction S.FMinus, 
                                                 S.mkVariable "y",
                                                 S.mkVariable "x"]),
              S.mkConstant (S.toConstant (3 :: Int)),
              S.mkConstant (S.toConstant (4 :: Int)) 
            ]

      print (SC.compileExpr lam)

    it "p225: compiles trinary sc" $ do
      -- (\x. (\y. (\z. + x y z))) 3 4 5
      -- > (\x. (\y. $1 y x))
      -- > (\x. $2 x)
      -- =>
      -- > $3 x = $2 x
      -- > $2 y x = $1 y x
      -- > $1 z y x = + x y z
      -- > ----------------
      -- > $3 3 4 5
      --
      -- > $1 z y x = + x y z
      -- > ----------------
      -- > $1 3 4 5
      let lam = S.mkApply [
              S.mkLambda ["x", "y", "z"] 
                         (S.mkApply [S.mkFunction S.FPlus, 
                                     S.mkVariable "x",
                                     S.mkVariable "y",
                                     S.mkVariable "z"]),
              S.mkConstant (S.toConstant (3 :: Int)),
              S.mkConstant (S.toConstant (4 :: Int)),
              S.mkConstant (S.toConstant (5 :: Int))
            ]

      putStrLn "TRINARY"
      print (SC.compileExpr lam)

    it "p237: compiles recursive lets" $ do
      {-
      letrec sumInts = \m. letrec count = \n. if (> n m) Nil (Cons n (count (+ n 1)))
                               in sum (count 1)
             sum     = \ns. if (= ns Nil) 0 (+ (head ns) (sum (tail ns)))
      in sumInts 100


      EXPECTED

      $count count m n = IF (> n m) NIL (CONS n (count (+ n 1)))
      $sum ns = IF (= ns NIL) 0 (+ (head ns) ($sum (tail ns)))
      $sumInts m = letrec count = $count count m
                   in $sum (count 1)
      -------------------------------------------
      $sumInts 100

      -}
      putStrLn "RECURSIVE"
      print (SC.compileExpr sumInts_prog)
    
    it "p296: compiles high level syntax" $ do
      mir_src <- readMiranda "supercombs/simple"
      prog <- parseHunit mir_src :: IO M.Prog
      let l_expr = Trans.progToLambda prog
      putStrLn "HIGH LEVEL COMPILATION"
      print l_expr
      putStrLn "======="
      print $ Comp.lexprToSCProg (Trans.progToLambda prog)


sumInts_prog :: S.Exp 
sumInts_prog = 
  S.Letrec 
    [ ("sumInts", sumInts), ("sum", sum_e)]
    (S.Apply (S.mkVariable "sumInts") (S.mkConstant (S.CNat 100)))
  where
    sumInts = 
      S.Lambda "m" 
        (S.Letrec 
          [ ( "count", 
              S.Lambda "n"
               (S.mkApply
                 [ S.mkFunction S.FIf
                 , S.mkApply [ S.mkFunction S.FGt
                             , S.mkVariable "n"
                             , S.mkVariable "m"
                             ]
                 , S.mkConstant S.CNil
                 , S.mkApply 
                     [ S.mkFunction S.FCons
                     , S.mkVariable "n"
                     , S.Apply 
                         (S.mkVariable "count")
                         (S.mkApply 
                           [ S.mkFunction S.FPlus 
                           , S.mkVariable "n"
                           , S.mkConstant (S.CNat 1)
                           ])
                     ]
                 ])
            )
          ]
          (S.Apply 
            (S.mkVariable "sum")
            (S.Apply (S.mkVariable "count") (S.mkConstant (S.CNat 1)))))

    sum_e =
      S.Lambda "ns" $
          S.mkApply 
            [ S.mkFunction S.FIf
            , S.mkApply 
                [ S.mkFunction S.FEq
                , S.mkVariable "ns"
              , S.mkConstant S.CNil
              ]
          , S.mkConstant (S.CNat 0)
          , S.mkApply 
              [ S.mkFunction S.FPlus
              , S.Apply 
                  (S.mkVariable "head")
                  (S.mkVariable "ns")
              , S.Apply 
                  (S.mkVariable "sum")
                  (S.Apply 
                    (S.mkVariable "tail")
                    (S.mkVariable "ns"))
              ]
          ]

{-
letrec sumInts = \m. letrec count = \n. IF (> n m) NIL (CONS n (count (+ n 1)))
                         in sum (count 1)
       sum     = \ns. IF (= ns NIL) 0 (+ (HEAD ns) (sum (TAIL ns)))
in sumInts 100


$1 m count n = IF (> n m) NIL (CONS n (count (+ n 1)))
---
letrec sumInts = \m. letrec count = $1 m count
                         in sum (count 1)
       sum     = \ns. IF (= ns NIL) 0 (+ (HEAD ns) (sum (TAIL ns)))
in sumInts 100


$1 m count n = IF (> n m) NIL (CONS n (count (+ n 1)))
$m 
-}
