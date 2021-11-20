module TypeCheckSpec (spec) where

import Test.Hspec
import Syntax
import TypeChecker

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "typeofExpr (binary ops)" $ do
      let binop = BinOp Plus (Number 1) (Number 2)
      typeofExpr binop `shouldBe` Right (NumType "Num")
      let binop = BinOp Plus (BinOp Minus (Number 4) (BinOp Times (Number 1) (Number 5))) (Number 2)
      typeofExpr binop `shouldBe` Right (NumType "Num")
      let binop = BinOp GreaterThan (BinOp Minus (Number 4) (BinOp Times (Number 1) (Number 5))) (Number 2)
      typeofExpr binop `shouldBe` Right (NumType "Bool")

