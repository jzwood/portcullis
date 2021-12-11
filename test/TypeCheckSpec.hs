module TypecheckSpec (spec) where

import Test.Hspec
import Syntax
import Typecheck
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "typeofExpr (binary ops)" $ do
      let binop = BinOp Plus (Prim $ Number 1) (Prim $ Number 2)
      typeofExpr Map.empty binop `shouldBe` Right NumType
      let binop = BinOp Plus (BinOp Minus (Prim $ Number 4) (BinOp Times (Prim $ Number 1) (Prim $ Number 5))) (Prim $ Number 2)
      typeofExpr Map.empty binop `shouldBe` Right (NumType)
      let binop = BinOp GreaterThan (BinOp Minus (Prim $ Number 4) (BinOp Times (Prim $ Number 1) (Prim $ Number 5))) (Prim $ Number 2)
      typeofExpr Map.empty binop `shouldBe` Right AtomType

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
      typecheckExpr t ts `shouldBe` te
