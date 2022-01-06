module ParserSpec (spec) where

import Test.Hspec
import MiniParser
import Syntax
import Parser

success :: Either ParseError (a, Cursor, String) -> Maybe (a, String)
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
      let result = runParser parseStmt mempty "divide ->   -> Num Num   Num   \n divide   num den   = /   num den  "
          expected = Function "divide" (Arrow (Arrow NumType NumType) NumType) ["num","den"] (BinOp Divide (Ident "num") (Ident "den"))
      success result `shouldBe` Just (expected, "")
