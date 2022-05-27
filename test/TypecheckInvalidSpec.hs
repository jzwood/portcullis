module TypecheckInvalidSpec (spec) where

import Test.Hspec
import Data.Function
import Data.Functor
import Data.Either (isLeft)
import Syntax
import CodeGen
import Parser
import Typecheck
import Compile
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

    it "typeExprToList" $ do
      typeExprToList NumType `shouldBe` [NumType]
      typeExprToList (Arrow AtomType CharType) `shouldBe` [AtomType, CharType]
      typeExprToList (Arrow AtomType CharType) `shouldBe` [AtomType, CharType]
      typeExprToList (Arrow (Arrow NumType AtomType) CharType) `shouldBe` [Arrow NumType AtomType, CharType]
      typeExprToList (Arrow (Arrow NumType AtomType) (Arrow AtomType CharType)) `shouldBe` [Arrow NumType AtomType, AtomType, CharType]

    it "typecheck neg" $ do
      let stmt = Function { name = "neg"
                          , signature = Arrow NumType NumType
                          , args = ["x"]
                          , body = BinOp Minus (Val $ Number 0) (Ident "x")
                          }
          m = Map.singleton "neg" stmt
      typeofExpr m stmt (body stmt) `shouldBe` Right NumType
      typecheckStmt m stmt `shouldBe` Right NumType
