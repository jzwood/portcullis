module TypecheckTypeErrorsSpec (spec) where

import Test.Hspec
import Data.Function
import Data.Functor
import Data.Either (isLeft, isRight)
import Data.List
import Data.Set (Set)
import Syntax
import qualified Data.Set as Set
import Typecheck hiding (TypecheckError)
import qualified Typecheck as T
import qualified Compile as C
import Util


compile :: String -> Either String String
compile program = mapLeft show (C.compile program)

errorOf :: String -> T.TypecheckError
errorOf program = case C.compile program of
  Left (C.TypecheckError [typecheckError]) -> typecheckError
  _ -> error "program expected to raise 1 TypecheckError (but didn't)"

errorsOf :: String -> Set T.TypecheckError
errorsOf program = case C.compile program of
  Left (C.TypecheckError typecheckErrors) -> Set.fromList typecheckErrors
  _ -> error "program expected to raise TypecheckErrors (but didn't)"

unsafeCompile :: String -> Module
unsafeCompile program =
  case C.parse program of
    Right mod -> mod
    Left _ -> error "program unexpectedly failed to parse"

unsafeFunc :: String -> Function
unsafeFunc program = head . functions $ unsafeCompile program

expectLeft :: String -> String
expectLeft program =
  case compile program of
    Left err -> err
    Right _ -> error "program expected to error did not"

spec :: Spec
spec = do
  describe "Typecheck Function TypeErrors" $ do

    it "expect NotFunction" $ do
      let bad1 = "bad1 -> Num a\
                 \bad1 x = x"
          bad2 = "bad2 -> -> a b -> a b\
                 \bad2 f x = (f x)"
          bad3 = "bad3 -> -> Num Atom -> Num Num \
                 \ bad3 f x = (bad2 f x)"
      errorOf bad1 `shouldBe` FunctionError (unsafeFunc bad1) (NotFunction "x")
      errorOf bad2 `shouldBe` FunctionError (unsafeFunc bad2) (NotFunction "f")
      errorOf bad3 `shouldBe` FunctionError (unsafeFunc bad3) (NotFunction "bad2")

    it "expect TypeMismatch" $ do
      let bad0 = "bad0 -> z Num\
                 \bad0 x = x"
          bad1 = "bad1 -> Num -> Num Num\
                 \bad1 p = p"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) (TypeMismatch {expected = NumType, actual = Unspecfied "z"})
      errorOf bad1 `shouldBe` FunctionError (unsafeFunc bad1) (TypeMismatch {expected = Arrow NumType NumType, actual = NumType})

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
                \| sum [&a1 &b1] &c1  # pipe #"
          sum = "sum -> Num -> Num Num\
                \sum a b = + a b"
      expectLeft pipesQueues `shouldSatisfy` ("NotFunction" `isInfixOf`)
      compile (pipesQueues ++ '\n' : sum) `shouldSatisfy` isRight

    it "expect DuplicateQueue pipe errors" $ do
      let pipesQueues = "&a1 4 Num\
                        \&a1 2 Char"
      expectLeft pipesQueues `shouldSatisfy` ("DuplicateQueue" `isInfixOf`)

{-
    it "expect QueueNotFound pipe errors" $ do
      let pipe = "id -> z z\
                 \id x = x"
      expectLeft pipe `shouldSatisfy` ("QueueNotFound" `isInfixOf`)

-}
