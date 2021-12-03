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

  -- applyType :: TypeExpr -> TypeExpr -> TypeExpr -> Either TypeError TypeExpr
    it "appyType" $ do
      let t = NumType
          t1 = NumType
          t2 = AtomType
      applyType t t1 t2 `shouldBe` Right t2
      let t = NumType
          t1 = NumType
          t2 = AtomType
      applyType t t1 t2 `shouldBe` Right t2

    it "concretize" $ do
      concretize (\n -> AtomType) NumType `shouldBe` NumType
      concretize (\n -> AtomType) (Arrow (Unspecfied "b") CharType) `shouldBe` (Arrow AtomType CharType)
      concretize (\n -> if n == "a" then NumType else Unspecfied n) (Arrow (Unspecfied "a") (Unspecfied "b")) `shouldBe` (Arrow NumType (Unspecfied "b"))
