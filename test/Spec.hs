{-# LANGUAGE QuasiQuotes #-}

module Main where 

import Test.Hspec 
import Test.HUnit.Base (assertFailure)
import Text.RawString.QQ(r)

import Lambda.Parser (parseExpression)
import Lambda.Syntax (ToLambda (..), showMarked)
import Lambda.FreeBound (markBoundFree)
import Lambda.Pretty (PrettyLambda (..))
import Lambda.Reduce (reduce)
import Lambda.Eval (eval)

main :: IO ()
main = hspec $ do 
  describe "<= 2.4" $ do 

    it "p9: parses '(+ 4 5)'" $ do 
      showParsed "(+ 4 5)" `ioShouldBe` "+ 4 5"

    it "p10: parses '(+ (* 5 6) (* 8 3))'" $ do 
      showParsed "(+ (* 5 6) (* 8 3))" `ioShouldBe` "+ (* 5 6) (* 8 3)"

    it "p10: reduces '(+ (* 5 6) (* 8 3))'" $ do 
      showReduced "(+ (* 5 6) (* 8 3))" `ioShouldBe` "54"

    it "p10: reduces explicit currying '((+ 3) 4)'" $ do 
      showReduced "((+ 3) 4)" `ioShouldBe` "7"

    it "p12: reduces AND 'AND TRUE FALSE'" $ do 
      showReduced "AND TRUE FALSE" `ioShouldBe` "FALSE"

    it "p12: reduces IF 'IF TRUE 1 2' && 'IF FALSE 1 2'" $ do
      showReduced "IF TRUE 1 2" `ioShouldBe` "1"
      showReduced "IF FALSE 1 2" `ioShouldBe` "2"

    it "p12: reduces CONS access '(CONS 1 2)'" $ do 
      showReduced "HEAD (CONS 1 2)" `ioShouldBe` "1"
      showReduced "TAIL (CONS 1 2)" `ioShouldBe` "2"

    it "p13: reduces lambda abstr '(\\x. + x 1) 4" $ do
      showReduced "(\\x. + x 1) 4" `ioShouldBe` "5"

    it "p14: identifies bound and free" $ do
      showMarked' "(\\x. + x y) 4" `ioShouldBe` "(\\x:b. + x:b y:f) 4"
      showMarked' "\\x. + ((\\y. + y z) 7) x" `ioShouldBe` "\\x:b. + ((\\y:b. + y:b z:f) 7) x:b"
      showMarked' "+ x ((\\x. + x 1) 4)" `ioShouldBe` "+ x:f ((\\x:b. + x:b 1) 4)"

    it "p15: reduces simple lambda" $ do 
      showReduced "(\\x. + x 1) 4" `ioShouldBe` "5"

    it "p16: reduces multiple occurrence lambda" $ do
      showReduced "(\\x. + x x) 5" `ioShouldBe` "10"

    it "p16: reduces nested lambdas" $ do
      showReduced "(\\x.(\\y. - y x)) 4 5" `ioShouldBe` "1"

    it "p16: reduces lambda func app" $ do 
      showReduced "(\\f. f 3) (\\x. + x 1)" `ioShouldBe` "4"

    it "p17: accounts for nested lambda var conflicts" $ do 
      showReduced "(\\x.(\\x. + (- x 1)) x 3) 9" `ioShouldBe` "11"
      showReduced "(\\x.(\\x. x)) 1 2" `ioShouldBe` "2"

    it "p17: purely functional cons" $ do 
      -- HEAD (CONS p q) = CONS p q (\a.\b.a) = (\a.\b.\f. f a b) p q (\a.\b.a) 
      showReduced [r|(\a.\b.\f. f a b) p q (\a.\b.a)|] `ioShouldBe` "p"

    it "p19: eta reduces simple function application" $ do 
      showReduced "(\\x. + 1 x)" `ioShouldBe` "+ 1"

    it "p20: equivalence by applying arbitrary argument" $ do 
      ioShouldBe (showReduced "If True ((\\p.p) 3) w") =<< showReduced "(\\x.3) w"

    it "p21: resolving name capture by alpha-conversion" $ do 
      showReduced "(\\f.\\x. f x) x" `ioShouldBe` "\\y. x y"
      showReduced "(\\f.\\x. f (f x)) x" `ioShouldBe` "\\y. x (x y)"

    it "p21: accounts for partial name-capture" $ do 
      showReduced "(\\x. x (\\y. x y)) y" `ioShouldBe` "y (\\z. y z)"
    
    it "p21: isn't too eager to alpha convert" $ do 
      showReduced "(\\x. x (\\y. y)) y" `ioShouldBe` "y (\\y. y)"

    it "p27: evaluates recursive fibonacci" $ do 
      showReduced [r|(\h.(\x. h (x x)) (\x. h (x x))) (\fac.\n. If (= n 0) 1 (* n (fac (- n 1)))) 4|] 
        `ioShouldBe` "24"

    it "p28: supports builtin Y combinator" $ do 
      showReduced "Y (\\fac.\\n. IF (= n 0) 1 (* n (fac (- n 1)))) 4"
        `ioShouldBe` "24"
  
  describe "2.5 The Denotational Semantics" $ do 
    it "p29: performs simple eval" $ do 
      showEvaled "+ 3 4" `ioShouldBe` "7" 

  describe "3.2 The Enriched Lambda Calculus" $ do
    it "p41: evaluates simple let expression" $ do
      showReduced "let x = 3 in (* x x)" `ioShouldBe` "9"
    it "p41: evaluates let expression in lambda expr" $ do 
      showReduced "+ 1 (let x = 3 in (* x x))" `ioShouldBe` "10"
    it "p41: evaluates nested let expression" $ do 
      showReduced "let x = 3 in (let y = 4 in (* x y))" `ioShouldBe` "12"
    it "p41: evaluates multiple let expression" $ do 
      showReduced "let x = 3\n    y = 4\n in (* x y)" `ioShouldBe` "12"
    it "evaluates multiple single line let expression" $ do 
      showReduced "let x = 3; y = 4 in (* x y)" `ioShouldBe` "12"
  
  describe "3.2.2 Simple letrec Expressions" $ do
    it "p42: evaluates fib letrec" $ do 
      showReduced "letrec factorial = \\n. IF (= n 0) 1 (* n (factorial (- n 1))) in factorial 4"
       `ioShouldBe` "24"

  describe "3.3 Translating Miranda" $ do
    it "p44: evaluates simple program" $ do 
      evalAndShowMiranda "square n = n * n\n2 * (square 5)"
        `ioShouldBe` "50"

  where 
    ioShouldBe :: (Show a, Eq a) => IO a -> a -> IO ()
    ioShouldBe io val = (`shouldBe` val) =<< io

    showParsed = (pShow <$>) . parseExpr
    showReduced = (pShow . reduce <$>) . parseExpr
    showEvaled = (pShow . eval <$>) . parseExpr
    showMarked' = (showMarked . markBoundFree . toLambda <$>) . parseExpr

    evalAndShowMiranda = undefined

    parseExpr input = case parseExpression input of
                        (Left err) -> assertFailure err 
                        (Right expr) -> return expr