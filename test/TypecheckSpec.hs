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
      errorOf bad0 `shouldBe` FunctionError (unsafeFunc bad0) (TypeMismatch {expected = NumType, actual = Unspecfied "z"})
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

  describe "Typecheck modules without TypecheckErrors" $ do
    it "typecheck simple functions" $ do
      let good0 = "neg -> Num Num \
                  \neg x = - 0 x \n\
                  \id -> w w \
                  \id q = q"
      compile good0 `shouldSatisfy` isRight
    it "typecheck no arg functions" $ do
      let good0 = "one Num \
                  \one = 1 \n\
                  \two -> Num Num \
                  \two x1 = + 1 (one)"
          good1 = "empty0 [Num] \
                  \empty0 = Num []\
                  \empty1 [x] \
                  \empty1 = x []"
      compile good0 `shouldSatisfy` isRight
      compile good1 `shouldSatisfy` isRight

{-
    it "typecheck id" $ do
      let id1 = Function { name = "id1"
                         , signature = Arrow (Unspecfied "z") (Unspecfied "z")
                         , args = ["val"]
                         , body = Ident "val"
                         }
          id2 = Function { name = "id2"
                         , signature = Arrow (Unspecfied "p") (Unspecfied "p")
                         , args = ["x"]
                         , body = Call "id1" [Ident "x"]
                         }
          m = Map.fromList [("id1", id1), ("id2", id2)]
      typeofExpr m id1 (body id1) `shouldBe` Right (Unspecfied "z")
      typecheckFunc m id1 `shouldBe` Right (Unspecfied "z")
      typeofExpr m id2 (body id2) `shouldBe` Right (Unspecfied "p")
      typecheckFunc m id2 `shouldBe` Right (Unspecfied "p")

    it "typecheck avg" $ do
      let func = Function { name = "avg"
                          , signature = Arrow NumType (Arrow NumType NumType)
                          , args = ["num1", "num2"]
                          , body = BinOp Times (BinOp Plus (Ident "num1") (Ident "num2")) (Val $ Number 0.5)
                          }
          m = Map.singleton "avg" func
      typeofExpr m func (body func) `shouldBe` Right NumType
      typecheckFunc m func `shouldBe` Right NumType


    it "typecheck call vs ident function" $ do
      let one1 = Function { name = "one1"
                          , signature = NumType
                          , args = []
                          , body = Val $ Number 1
                          }
          one2 = Function { name = "one2"
                          , signature = NumType
                          , args = []
                          , body = Call "one1" []
                          }
          one3 = Function { name = "one3"
                          , signature = NumType
                          , args = []
                          , body = Ident "one1"
                          }
          m = Map.fromList [("one1", one1), ("one2", one2), ("one3", one3)]
      typecheckFunc m one1 `shouldBe` Right NumType
      typecheckFunc m one2 `shouldBe` Right NumType

      typecheckFunc m one3 `shouldBe` Right NumType  -- HERE

    -- TODO: redo to not include fallback param
    it "typecheck map" $ do
      let map' = Function { name = "map"
                          , signature = Arrow (Arrow (Unspecfied "x") (Unspecfied "y")) (Arrow (Unspecfied "x") (Unspecfied "y"))
                          , args = ["f", "a"]
                          , body = Call "f" [Ident "a"]
                          }
          m = Map.fromList [("map", map')]
      typeofExpr m map' (body map') `shouldBe` Right (Unspecfied "y")
      typecheckFunc m map' `shouldBe` Right (Unspecfied "y")

    it "typecheck compose" $ do
      let id1 = Function { name = "id1"
                         , signature = Arrow (Unspecfied "z") (Unspecfied "z")
                         , args = ["val"]
                         , body = Ident "val"
                         }
          add1 = Function { name = "add1"
                          , signature = Arrow NumType NumType
                          , args = ["n"]
                          , body = BinOp Plus (Val $ Number 1) (Ident "n")
                          }
          compose = Function { name = "compose"
                             , signature = Arrow (Arrow (Unspecfied "b") (Unspecfied "c")) (Arrow (Arrow (Unspecfied "a") (Unspecfied "b")) (Arrow (Unspecfied "a") (Unspecfied "c")))
                             , args = ["f", "g", "x"]
                             , body = Call "f" [Call "g" [Ident "x"]]
                             }
          id2 = Function { name = "id2"
                         , signature = Arrow (Unspecfied "y") (Unspecfied "y")
                         , args = ["z"]
                         , body = Call "compose" [Ident "id1", Ident "id1", Ident "z"]
                         }
          add2 = Function { name = "add2"
                          , signature = Arrow NumType NumType
                          , args = ["n"]
                          , body = Call "compose" [ Ident "add1", Ident "add1", Ident "n"]
                          }
          m = Map.fromList [("id1", id1), ("id1", id1), ("compose", compose), ("add1", add1)]
      typeofExpr m compose (body compose) `shouldBe` Right (Unspecfied "c")
      typecheckFunc m compose `shouldBe` Right (Unspecfied "c")
      typeofExpr m id2 (body id2) `shouldBe` Right (Unspecfied "y")
      typecheckFunc m compose `shouldBe` Right (Unspecfied "c")
      typeofExpr m add2 (body add2) `shouldBe` Right NumType
      typecheckFunc m add2 `shouldBe` Right NumType

    it "typecheck guards" $ do
      let trivial = Function { name = "trivial"
                             , signature = Arrow AtomType CharType
                             , args = ["atom"]
                             , body = TernOp If ( BinOp Equal (Val $ Atom "Cat") (Ident "atom")) (Val $ Character 'y') (Val $ Character 'n')
                             }
          m = Map.fromList [("trivial", trivial)]
      typeofExpr m trivial (body trivial) `shouldBe` Right CharType
      typecheckFunc m trivial `shouldBe` Right CharType

    it "typecheck guards 2" $ do
      let hof1 = Function { name = "hof1"
                          , signature = Arrow (Arrow (Unspecfied "a") AtomType) (Arrow (ListType NumType) (ListType NumType))
                          , args = ["f", "xs"]
                          , body = TernOp If (Call "f" [Ident "xs"]) (Ident "xs") (Ident "xs")
                          }
          hof2 = Function { name = "hof2"
                          , signature = Arrow (Arrow (Unspecfied "a") AtomType) (Arrow (ListType NumType) (ListType NumType))
                          , args = ["f", "xs"]
                          , body = Call "f" [Ident "xs"]
                          }
          m = Map.fromList [("hof1", hof1), ("hof2", hof2)]
      isLeft (typecheckFunc m hof2) `shouldBe` True
      typecheckFunc m hof1 `shouldBe` Right (ListType NumType)
-}
