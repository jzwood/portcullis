module TypecheckSpec (spec) where

import Test.Hspec
import Data.Function
import Data.Functor
import Data.Either (isLeft)
import Syntax
import CompileTarget
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

    -- typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
    --it "typecheck" $ do
      --Typecheck.typecheck NumType (Arrow NumType NumType) Map.empty `shouldBe` Right (Map.fromList [])

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

    it "typecheck length" $ do
      let stmt = Function { name = "len"
                          , signature = Arrow (ListType (Unspecfied "x")) NumType
                          , args = ["xs"]
                          , body = UnOp Length (Ident "xs")
                          }
          m = Map.singleton "len" stmt
      typeofExpr m stmt (body stmt) `shouldBe` (Right NumType)
      typecheckStmt m stmt `shouldBe` (Right NumType)

    it "typecheck tail different unspecified naming" $ do
      let stmt = Function { name = "tail"
                          , signature = Arrow (ListType (Unspecfied "x")) (ListType (Unspecfied "x"))
                          , args = ["xs"]
                          , body = TernOp Slice (Ident "xs") (Val $ Number 1) (UnOp Length (Ident "xs"))
                          }
          m = Map.singleton "tail" stmt
      typeofExpr m stmt (body stmt) `shouldBe` (Right $ ListType (Unspecfied "x"))
      typecheckStmt m stmt `shouldBe` (Right $ ListType (Unspecfied "x"))

    it "typecheck empty" $ do
      let stmt = Function { name = "empty"
                          , signature = ListType NumType
                          , args = []
                          , body = Val $ List NumType []
                          }
          m = Map.singleton "empty" stmt
      typeofExpr m stmt (body stmt) `shouldBe` (Right $ ListType NumType)
      typecheckStmt m stmt `shouldBe` (Right $ ListType NumType)

    it "typecheck no arg function" $ do
      let one = Function { name = "one"
                          , signature = NumType
                          , args = []
                          , body = Val $ Number 1.0
                          }
          two = Function { name = "two"
                          , signature = Arrow NumType NumType
                          , args = ["x1"]
                          , body = BinOp Plus (Val $ Number 1) (Call "one" [])
                          }
          m = Map.fromList [("two", two), ("one", one)]
      typeofExpr m one (body one) `shouldBe` (Right NumType)
      typecheckStmt m one `shouldBe` (Right NumType)
      typeofExpr m two (body two) `shouldBe` (Right NumType)
      typecheckStmt m two `shouldBe` (Right NumType)

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
      typeofExpr m id1 (body id1) `shouldBe` (Right $ Unspecfied "z")
      typecheckStmt m id1 `shouldBe` (Right $ Unspecfied "z")
      typeofExpr m id2 (body id2) `shouldBe` (Right $ Unspecfied "p")
      typecheckStmt m id2 `shouldBe` (Right $ Unspecfied "p")

    it "typecheck avg" $ do
      let stmt = Function { name = "avg"
                          , signature = Arrow NumType (Arrow NumType NumType)
                          , args = ["num1", "num2"]
                          , body = BinOp Times (BinOp Plus (Ident "num1") (Ident "num2")) (Val $ Number 0.5)
                          }
          m = Map.singleton "avg" stmt
      typeofExpr m stmt (body stmt) `shouldBe` (Right NumType)
      typecheckStmt m stmt `shouldBe` (Right NumType)

    it "typecheck sum and mean" $ do
      let tail = Function { name = "tail"
                          , signature = Arrow (ListType (Unspecfied "lst")) (ListType (Unspecfied "lst"))
                          , args = ["xs"]
                          , body = TernOp Slice (Ident "xs") (Val $ Number 1) (UnOp Length (Ident "xs"))
                          }
          sum = Function { name = "sum"
                          , signature = Arrow (ListType NumType) NumType
                          , args = ["xs"]
                          , body = Guard [(BinOp Equal (Val $ Number 0) (UnOp Length (Ident "xs")), Val $ Number 1)] (BinOp Plus (TernOp At (Ident "xs") (Val $ Number 0) (Val $ Number 0)) (Call "sum" [Call "tail" [Ident "xs"]]))
                          }
          mean = Function { name = "mean"
                          , signature = Arrow (ListType NumType) NumType
                          , args = ["nums"]
                          , body = BinOp Divide (Call "sum" [Ident "nums"]) (UnOp Length (Ident "nums"))
                          }
          m = Map.fromList [("tail", tail), ("mean", mean), ("sum", sum)]
      typeofExpr m tail (body tail) `shouldBe` (Right $ ListType (Unspecfied "lst"))
      typecheckStmt m tail `shouldBe` (Right $ ListType (Unspecfied "lst"))
      typeofExpr m sum (body sum) `shouldBe` (Right NumType)
      typecheckStmt m sum `shouldBe` (Right NumType)
      typeofExpr m mean (body mean) `shouldBe` (Right NumType)
      typecheckStmt m mean `shouldBe` (Right NumType)


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
      typecheckStmt m one1 `shouldBe` (Right NumType)
      typecheckStmt m one2 `shouldBe` (Right NumType)

      typecheckStmt m one3 `shouldBe` (Right NumType)  -- HERE
      --isLeft (typecheckStmt m one3) `shouldBe` True  -- HERE

    it "typecheck map" $ do
      let map' = Function { name = "map"
                          , signature = Arrow (Arrow (Unspecfied "x") (Unspecfied "y")) (Arrow (Unspecfied "x") (Unspecfied "y"))
                          , args = ["f", "a"]
                          , body = Call "f" [Ident "a"]
                          }
          m = Map.fromList [("map", map')]
      typeofExpr m map' (body map') `shouldBe` (Right $ Unspecfied "y")
      typecheckStmt m map' `shouldBe` (Right $ Unspecfied "y")

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
      typeofExpr m compose (body compose) `shouldBe` (Right $ Unspecfied "c")
      typecheckStmt m compose `shouldBe` (Right $ Unspecfied "c")
      typeofExpr m id2 (body id2) `shouldBe` (Right $ Unspecfied "y")
      typecheckStmt m compose `shouldBe` (Right $ Unspecfied "c")
      typeofExpr m add2 (body add2) `shouldBe` (Right NumType)
      typecheckStmt m add2 `shouldBe` (Right NumType)
