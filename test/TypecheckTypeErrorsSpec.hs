module TypecheckTypeErrorsSpec (spec) where

import Test.Hspec
import Data.Function
import Data.Functor
import Data.Either (isLeft, isRight)
import Data.List
import qualified Compile as C
import Util

compile :: String -> Either String String
compile program = mapLeft show (C.compile program)

expectLeft :: String -> String
expectLeft program =
  case compile program of
    Left err -> err
    Right _ -> error "function expected to error did not"

--data TypeError
  -- NotFunction Name  -- maybe make better show
  -- DuplicateFunction
  -- TypeMismatch TypeExpr TypeExpr
  -- ArityMismatch

spec :: Spec
spec = do
  describe "Typecheck Function TypeErrors" $ do

    it "expect NotFound" $ do
      let bad1 = "bad1 -> Num a\
                 \bad1 x = x"
          bad2 = "bad2 -> -> a b -> a b\
                 \bad2 f x = (f x)"
          bad3 = "bad3 -> -> Num Atom -> Num Num \
                 \ bad3 f x = (bad2 f x)"
      expectLeft bad1 `shouldSatisfy` ("NotFunction" `isInfixOf`)
      expectLeft bad2 `shouldSatisfy` ("NotFunction" `isInfixOf`)
      expectLeft bad3 `shouldSatisfy` ("NotFunction" `isInfixOf`)

    it "expect TypeMismatch" $ do
      let bad0 = "bad0 -> z Num\
                 \bad0 x = x"
          bad1 = "bad -> Num -> Num -> Num -> Num Num\
                 \good p = p"
      expectLeft bad0 `shouldSatisfy` ("TypeMismatch Num z" `isInfixOf`)
      expectLeft bad1 `shouldSatisfy` ("TypeMismatch (Num -> (Num -> (Num -> Num))) Num" `isInfixOf`)

    it "expect DuplicateFunction" $ do
      let bad0 = "good -> Atom Num\
                 \good a = 3\n\
                 \good [Num]\
                 \good = Num [3]"
      expectLeft bad0 `shouldSatisfy` ("DuplicateFunction" `isInfixOf`)

    it "expect ArityMismatch" $ do
      let bad0 = "bad -> Num Num\
                 \good a b c d e f = a"
      expectLeft bad0 `shouldSatisfy` ("ArityMismatch" `isInfixOf`)

  describe "Typecheck Pipe TypeErrors" $ do
    it "expect NotFunction pipe errors" $ do
      let pipesQueues = "&a1 4 Num  # queue #\
                \&b1 4 Num  # queue #\
                \&c1 4 Num  # queue #\n\
                \| sum [&a1 &b1] &c1  # pipe #\
                \| sum [&a1 &b1] &c1  # pipe #"
          sum = "sum -> Num -> Num Num\
                \sum a b = + a b"
      expectLeft pipesQueues `shouldSatisfy` ("NotFunction" `isInfixOf`)
      expectLeft (pipesQueues ++ '\n' : sum) `shouldSatisfy` ("" `isInfixOf`)

    it "expect DuplicateQueue pipe errors" $ do
      let pipesQueues = "&a1 4 Num\
                        \&a1 2 Char"
      expectLeft pipesQueues `shouldSatisfy` ("DuplicateQueue" `isInfixOf`)
