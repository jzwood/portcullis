module TypecheckSpec (spec) where

import Test.Hspec
import Data.Function
import Data.Functor
import Syntax
import CompileTarget
import Parser
import Typecheck
import Transpile
import qualified Data.Map as Map


spec :: Spec
spec = do
  describe "typecheck" $ do
    it "typeofExpr (binary ops)" $ do
      let dummyStatement = Function { name = "test" , signature = NumType , args = ["a"] , body = Val $ Number 1 }
          binop = BinOp Plus (Val $ Number 1) (Val $ Number 2)
      typeofExpr Map.empty dummyStatement binop `shouldBe` Right NumType
      let binop = BinOp Plus (BinOp Minus (Val $ Number 4) (BinOp Times (Val $ Number 1) (Val $ Number 5))) (Val $ Number 2)
      typeofExpr Map.empty dummyStatement binop `shouldBe` Right (NumType)
      let binop = BinOp GreaterThan (BinOp Minus (Val $ Number 4) (BinOp Times (Val $ Number 1) (Val $ Number 5))) (Val $ Number 2)
      typeofExpr Map.empty dummyStatement binop `shouldBe` Right AtomType

    it "typecheckExpr" $ do
      let t = NumType
          ts = Arrow NumType AtomType
          te = Right AtomType
      typecheckExpr t ts `shouldBe` te
      let t = Arrow NumType CharType
          ts = Arrow (Arrow NumType (Unspecfied "a")) AtomType
          te = Right AtomType
      typecheckExpr t ts `shouldBe` te
      let t = Arrow AtomType NumType
          ts = Arrow (Arrow (Unspecfied "a") (Unspecfied "b")) (Arrow (Unspecfied "a") CharType)
          te = Right $ Arrow AtomType CharType
      typecheckExpr t ts `shouldBe` te
      let t = Arrow AtomType CharType
          ts = Arrow (Arrow (Unspecfied "a") (Unspecfied "b")) (Arrow (Unspecfied "a") (Unspecfied "b"))
          te = Right $ Arrow AtomType CharType
      typecheckExpr t ts `shouldBe` te  -- same as previous but but with unspecified types named differently
      let t = Arrow AtomType CharType
          ts = Arrow (Arrow (Unspecfied "x") (Unspecfied "z")) (Arrow (Unspecfied "x") (Unspecfied "z"))
          te = Right $ Arrow AtomType CharType
      typecheckExpr t ts `shouldBe` te

    -- typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
    --it "typecheck" $ do
      --Typecheck.typecheck NumType (Arrow NumType NumType) Map.empty `shouldBe` Right (Map.fromList [])

    it "argsToList" $ do
      argsToList NumType `shouldBe` [NumType]
      argsToList (Arrow AtomType CharType) `shouldBe` [AtomType, CharType]
      argsToList (Arrow AtomType CharType) `shouldBe` [AtomType, CharType]
      argsToList (Arrow (Arrow NumType AtomType) CharType) `shouldBe` [Arrow NumType AtomType, CharType]
      argsToList (Arrow (Arrow NumType AtomType) (Arrow AtomType CharType)) `shouldBe` [Arrow NumType AtomType, AtomType, CharType]

    it "typecheck neg" $ do
      let stmt = Function { name = "neg"
                          , signature = Arrow NumType NumType
                          , args = ["x"]
                          , body = BinOp Minus (Val $ Number 0) (Ident "x")
                          }
          m = Map.singleton "neg" stmt
      typeofExpr m stmt (body stmt) `shouldBe` (Right NumType)
      typecheckStmt m stmt `shouldBe` (Right NumType)

    it "typecheck tail" $ do
      let stmt = Function { name = "tail"
                          , signature = Arrow (ListType (Unspecfied "a")) (ListType (Unspecfied "a"))
                          , args = ["xs"]
                          , body = TernOp Slice (Ident "xs") (Val $ Number 1) (UnOp Length (Ident "xs"))
                          }
          m = Map.singleton "tail" stmt
      typeofExpr m stmt (body stmt) `shouldBe` (Right $ ListType (Unspecfied "a"))
      typecheckStmt m stmt `shouldBe` (Right $ ListType (Unspecfied "a"))

    it "typecheck identity" $ do
      let stmt = Function { name = "id"
                          , signature = Arrow (Unspecfied "x") (Unspecfied "x")
                          , args = ["x"]
                          , body = Ident "x"
                          }
          m = Map.singleton "id" stmt
      typeofExpr m stmt (body stmt) `shouldBe` (Right $ Unspecfied "x")
      typecheckStmt m stmt `shouldBe` (Right $ Unspecfied "x")

    --it "typecheck length" $ do
      --let stmt = Function { name = "len"
                          --, signature = Arrow (ListType (Unspecfied "x")) NumType
                          --, args = ["xs"]
                          --, body = UnOp Length (Ident "xs")
                          --}
          --m = Map.singleton "leng" stmt
      --typeofExpr m stmt (body stmt) `shouldBe` (Right $ Unspecfied "x")

      --typecheckStmt m stmt `shouldBe` (Right $ Unspecfied "x")

    --it "typecheck tail different unspecified naming" $ do
      --let stmt = Function { name = "tail"
                          --, signature = Arrow (ListType (Unspecfied "x")) (ListType (Unspecfied "x"))
                          --, args = ["xs"]
                          --, body = TernOp Slice (Ident "xs") (Val $ Number 1) (UnOp Length (Ident "xs"))
                          --}
          --m = Map.singleton "tail" stmt
      --typeofExpr m stmt (body stmt) `shouldBe` (Right $ ListType (Unspecfied "a"))
      --typecheckStmt m stmt `shouldBe` (Right $ ListType (Unspecfied "a"))

--tail -> [a] [a]
--tail xs = !! xs 1 _ xs

--empty [Num]
--empty = Num []
