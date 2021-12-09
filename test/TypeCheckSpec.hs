module TypeCheckSpec (spec) where

import Test.Hspec
import Syntax
import TypeChecker
import qualified Data.Map as Map

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
      let te = (Arrow (Unspecfied "a") (Unspecfied "b"))
          t1 = NumType
          t2 = NumType
      applyType te t1 t2 `shouldBe` Left Mismatch
      --let te = (Arrow (Unspecfied "a") (Unspecfied "b"))
          --t1 = (Arrow (Unspecfied "a") (Unspecfied "b"))
          --t2 = NumType
      --applyType te t1 t2 `shouldBe` Right t2
      --let te = (Arrow (Unspecfied "a") (Unspecfied "b"))
          --t1 = (Arrow (Unspecfied "c") (Unspecfied "d"))
          --t2 = NumType
      --applyType te t1 t2 `shouldBe` Right t2
      --let te = (Arrow (Unspecfied "a") (Unspecfied "b"))
          --t1 = (Arrow (Unspecfied "c") (Unspecfied "d"))
          --t2 = NumType
      --applyType te t1 t2 `shouldBe` Right t2

    --it "specify NumType" $ do
      --specify ("a", NumType) (Arrow NumType (Unspecfied "a")) `shouldBe` (Arrow NumType NumType)
      --specify ("a", AtomType) (Arrow (Unspecfied "a") NumType) `shouldBe` (Arrow AtomType NumType)
      --specify ("a", NumType) (Arrow (Arrow (Unspecfied "b") (Unspecfied "a")) (Unspecfied "a"))
        --`shouldBe` (Arrow (Arrow (Unspecfied "b") AtomType) AtomType)

    --it "specify Function" $ do
      ---- specify ('f', a -> b) , (c -> d) -> d
      --specify ("f", (Arrow NumType CharType)) (Arrow (Unspecfied "f") NumType) `shouldBe` (Arrow NumType NumType)
      --specify ("a", NumType) (Arrow NumType (Unspecfied "a")) `shouldBe` (Arrow NumType NumType)
      --specify ("a", NumType) (Arrow (Arrow (Unspecfied "b") (Unspecfied "a")) (Unspecfied "a"))
        --`shouldBe` (Arrow (Arrow (Unspecfied "b") AtomType) AtomType)

    it "normalizeType" $ do
      let t = Arrow (Unspecfied "a") (Arrow (Unspecfied "b") (Unspecfied "a"))
          nt = Arrow (Unspecfied "a0") (Arrow (Unspecfied "a1") (Unspecfied "a0"))
      fst (normalizeTypeExpr Map.empty t) `shouldBe` nt
      let t = Arrow AtomType (Arrow (Unspecfied "b") (Unspecfied "a"))
          nt = Arrow AtomType (Arrow (Unspecfied "a0") (Unspecfied "a1"))
      fst (normalizeTypeExpr Map.empty t) `shouldBe` nt
