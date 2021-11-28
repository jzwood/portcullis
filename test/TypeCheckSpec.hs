module TypeCheckSpec (spec) where

import Test.Hspec
import Syntax
import TypeChecker

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "typeofExpr (binary ops)" $ do
      let binop = BinOp Plus (Prim $ Number 1) (Prim $ Number 2)
      typeofExpr binop `shouldBe` Right NumType
      let binop = BinOp Plus (BinOp Minus (Prim $ Number 4) (BinOp Times (Prim $ Number 1) (Prim $ Number 5))) (Prim $ Number 2)
      typeofExpr binop `shouldBe` Right (NumType)
      let binop = BinOp GreaterThan (BinOp Minus (Prim $ Number 4) (BinOp Times (Prim $ Number 1) (Prim $ Number 5))) (Prim $ Number 2)
      typeofExpr binop `shouldBe` Right AtomType

