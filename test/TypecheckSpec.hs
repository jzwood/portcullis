module TypecheckSpec (spec) where

import Test.Hspec
import Data.Function
import Data.Functor
import Data.Either (isLeft, isRight)
import Data.List
import Data.Set (Set)
import qualified Data.Map as Map
import Syntax
import qualified Data.Set as Set
import Typecheck hiding (TypecheckError)
import qualified Typecheck as T
import qualified Compile as C
import Util

compile :: String -> Either String String
compile program = mapLeft show (C.compile program <&> show)  -- we don't actually care about actual output codegen for these tests
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

unsafeStream :: String -> Stream
unsafeStream program = head . streams $ unsafeCompile program

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
          bad2 = "bad2 [Num]\
                 \bad2 = Num:[SomeAtom]"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) (TypeMismatch {expected = NumType, actual = Unspecified "bad0.z", typeMap = Map.empty})
      errorOf bad1 `shouldBe` FunctionError (unsafeFunc bad1) (TypeMismatch {expected = Arrow NumType NumType, actual = NumType, typeMap = Map.empty})
      errorOf bad2 `shouldBe` FunctionError (unsafeFunc bad2) (TypeMismatch {expected = NumType, actual = AtomType, typeMap = Map.empty})

    it "expect DuplicateFunction" $ do
      let bad0 = "good -> Atom Num \
                 \good a = 3 \
                 \good [Num] \
                 \good = Num : [3]"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) DuplicateFunction

    it "expect ArityMismatch" $ do
      let bad0 = "bad -> Num Num \
                 \good a b c d e f = a"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) ArityMismatch

    it "expect RecursiveType" $ do
      let bad0 = "bad0 -> -> t Atom -> [t] Num \
                 \bad0 f xs = ? (f xs) 1 2"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) (RecursiveType ["bad0.t"])

    it "expect BadForall" $ do
      let bad0 = "bad0 -> (-> t Atom) -> Num Atom\
                 \bad0 f xs = (f xs)"
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) (BadForall ["bad0.t"])

  describe "Typecheck Pipe TypeErrors" $ do
    it "expect NotFunction pipe errors" $ do
      let bad0 = "&a1 Num \
                 \&b1 Num \
                 \&c1 Num \
                 \| sum [&a1 2 &b1 4] &c1  # pipe #"
          sum = "sum -> Num -> Num Num \
                \sum a b = + a b"
          good = unlines [bad0, sum]
      errorOf bad0 `shouldBe` PipeError (unsafePipe bad0) (NotFunction "sum")
      compile good `shouldSatisfy` isRight

    it "expect DuplicateStream pipe errors" $ do
      let bad0 = "&a1 Num \
                 \&a1 Atom"
      errorOf bad0 `shouldBe` DuplicateStreamError (unsafeStream bad0)

    it "expect StreamNotFound pipe errors" $ do
      let good0 = "id -> z z \n\
                  \id z = z"
          bad0 = unlines [good0, "| id [&a 1] &b"]
      compile good0 `shouldSatisfy` isRight
      errorOf bad0 `shouldBe` PipeError (unsafePipe bad0) (StreamNotFound "&a")
