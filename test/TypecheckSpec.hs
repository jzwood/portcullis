module TypecheckSpec (spec) where

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
      typeofExpr Map.empty dummyStatement binop `shouldBe` Right NumType
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
      let func = Function { name = "neg"
                          , signature = Arrow NumType NumType
                          , args = ["x"]
                          , body = BinOp Minus (Val $ Number 0) (Ident "x")
                          }
          m = Map.singleton "neg" func
      typeofExpr m func (body func) `shouldBe` Right NumType
      typecheckFunc m func `shouldBe` Right NumType

    it "typecheck identity" $ do
      let func = Function { name = "id"
                          , signature = Arrow (Unspecfied "x") (Unspecfied "x")
                          , args = ["x"]
                          , body = Ident "x"
                          }
          m = Map.singleton "id" func
      typeofExpr m func (body func) `shouldBe` Right (Unspecfied "x")
      typecheckFunc m func `shouldBe` Right (Unspecfied "x")

    it "typecheck empty" $ do
      let func = Function { name = "empty"
                          , signature = ListType NumType
                          , args = []
                          , body = Val $ List NumType []
                          }
          m = Map.singleton "empty" func
      typeofExpr m func (body func) `shouldBe` Right (ListType NumType)
      typecheckFunc m func `shouldBe` Right (ListType NumType)

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
      typeofExpr m one (body one) `shouldBe` Right NumType
      typecheckFunc m one `shouldBe` Right NumType
      typeofExpr m two (body two) `shouldBe` Right NumType
      typecheckFunc m two `shouldBe` Right NumType

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
