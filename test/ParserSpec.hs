module ParserSpec (spec) where

import Test.Hspec
import Parser
import Syntax

success :: Either Cursor (a, Cursor, String) -> Maybe (a, String)
success (Left _) = Nothing
success (Right (a, _, b)) = Just (a, b)

spec :: Spec
spec = do
  describe "Parser" $ do
    it "camel" $ do
      let result = runParser camel mempty "catWorld"
      success result `shouldBe` Just ("catWorld", "")
      let result = runParser camel mempty "Cat"
      success result `shouldBe` Nothing

    it "pascal" $ do
      let result = runParser pascal mempty "CatWorld"
      success result `shouldBe` Just ("CatWorld", "")
      let result = runParser pascal mempty "catWorld"
      success result `shouldBe` Nothing

    it "trim" $ do
      let result = runParser (trim camel) mempty "  cat  "
      success result `shouldBe` Just ("cat", "")

    it "paren" $ do
      let result = runParser (paren camel) mempty "(cat)"
      success result `shouldBe` Just ("cat", "")
      let result = runParser (paren camel) mempty "(cat"
      success result `shouldBe` Nothing
      let result = runParser (paren . trim $ camel) mempty "(  cat  )"
      success result `shouldBe` Just ("cat", "")

    it "numbers" $ do
      let result = runParser integer mempty "123abc"
      success result `shouldBe` Just (123, "abc")
      let result = runParser number mempty "123.456abc"
      success result `shouldBe` Just (123.456, "abc")

    it "parse Stmt" $ do
      let strType = runParser parseStmt mempty "NonZero /=  0   x "
          parsedType = Type "NonZero" (Binomial NotEqual (Real 0.0) X)
      success strType `shouldBe` Just (parsedType, "")
      let strSig = runParser parseStmt mempty "divide  ->   -> Num NonZero   Num"
          parsedSig = Signature "divide" (Arrow (Arrow (NumType "Num") (NumType "NonZero")) (NumType "Num"))
      success strSig `shouldBe` Just (parsedSig, "")
      let strFunction = runParser parseStmt mempty "divide   num den   = /   num den  "
          parsedFunction = Function "divide" [Var "num",Var "den"] (BinOp Divide (Ident "num") (Ident "den"))
      success strFunction `shouldBe` Just (parsedFunction, "")

    it "parse guard" $ do
      let strGuardFunc = runParser parseStmt mempty "guard a =\n ? > a 1 34 \n ? == a 0 0 \n ? 1 0"
          parsedStmt = Function "guard" [Var "a"] (Guard [(BinOp GreaterThan (Ident "a") (Number 1.0),Number 34.0),(BinOp Equal (Ident "a") (Number 0.0),Number 0.0),(Number 1.0,Number 0.0)])
      success strGuardFunc `shouldBe` Just (parsedStmt, "")
