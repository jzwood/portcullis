module TypecheckSpec (spec) where

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
  Left (C.TypecheckError tes) -> error $ unwords ["program expected to raise 1 TypecheckError but actually raised many:", show tes]
  _ -> error "program expected to raise 1 TypecheckError but didn't raise any"

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

unsafePipe :: String -> Pipe
unsafePipe program = head . pipes $ unsafeCompile program

unsafeQueue :: String -> Queue
unsafeQueue program = head . queues $ unsafeCompile program

spec :: Spec
spec = do
  describe "typecheck modules with TypeErrors" $ do
    it "expect NotFunction" $ do
      let bad0 = "bad0 -> Num Num \
                 \bad0 x = (f x)"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) (NotFunction "f")

    it "expect TypeMismatch" $ do
      let bad0 = "bad0 -> z Num \
                 \bad0 x = x"
          bad1 = "bad1 -> Num -> Num Num\
                 \bad1 p = p"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) (TypeMismatch {expected = NumType, actual = Unspecified "bad0.z"})
      errorOf bad1 `shouldBe` FunctionError (unsafeFunc bad1) (TypeMismatch {expected = Arrow NumType NumType, actual = NumType})

    it "expect DuplicateFunction" $ do
      let bad0 = "good -> Atom Num \
                 \good a = 3 \
                 \good [Num] \
                 \good = Num [3]"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) DuplicateFunction

    it "expect ArityMismatch" $ do
      let bad0 = "bad -> Num Num \
                 \good a b c d e f = a"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) ArityMismatch

    it "expect RecursiveType" $ do
      let bad0 = "bad0 -> -> t Atom -> [t] Num \
                 \bad0 f xs = ? (f xs) 1 2"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) (RecursiveType [("bad0.t",ListType (Unspecified "bad0.t"))])

  describe "Typecheck Pipe TypeErrors" $ do
    it "expect NotFunction pipe errors" $ do
      let bad0 = "&a1 4 Num \
                 \&b1 4 Num \
                 \&c1 4 Num \
                 \| sum [&a1 &b1] &c1  # pipe #"
          sum = "sum -> Num -> Num Num \
                \sum a b = + a b"
          good = unlines [bad0, sum]
      errorOf bad0 `shouldBe` PipeError (unsafePipe bad0) (NotFunction "sum")
      compile good `shouldSatisfy` isRight

    it "expect DuplicateQueue pipe errors" $ do
      let bad0 = "&a1 4 Num \
                 \&a1 2 Char"
      errorOf bad0 `shouldBe` DuplicateQueueError (unsafeQueue bad0)

    it "expect QueueNotFound pipe errors" $ do
      let good0 = "id -> z z \n\
                  \id z = z"
          bad0 = unlines [good0, "| id [&a] &b"]
      compile good0 `shouldSatisfy` isRight
      errorOf bad0 `shouldBe` PipeError (unsafePipe bad0) (QueueNotFound "a")
