module ParserSpec (spec) where

import Test.Hspec
import MiniParser
import Syntax
import Parser
import Compile (runp)

success :: Either ParseError (a, Cursor, String) -> Maybe (a, String)
success (Left _) = Nothing
success (Right (a, _, b)) = Just (a, b)

spec :: Spec
spec = do
  describe "Parser" $ do
    it "camel" $ do
      let result = runp camel "catWorld"
      success result `shouldBe` Just ("catWorld", "")
      let result = runp camel "_catWorld"
      success result `shouldBe` Just ("_catWorld", "")
      let result = runp camel "Cat"
      success result `shouldBe` Nothing

    it "pascal" $ do
      let result = runp pascal "CatWorld"
      success result `shouldBe` Just ("CatWorld", "")
      let result = runp pascal "catWorld"
      success result `shouldBe` Nothing

    it "trim" $ do
      let result = runp (trim camel) "  cat  "
      success result `shouldBe` Just ("cat", "")

    it "paren" $ do
      let result = runp (paren camel) "(cat)"
      success result `shouldBe` Just ("cat", "")
      let result = runp (paren camel) "(cat"
      success result `shouldBe` Nothing
      let result = runp (paren . trim $ camel) "(  cat  )"
      success result `shouldBe` Just ("cat", "")

    it "numbers" $ do
      let result = runp integer "123abc"
      success result `shouldBe` Just (123, "abc")
      let result = runp number "123.456abc"
      success result `shouldBe` Just (123.456, "abc")

    it "parse Func" $ do
      let result = runp parseFunc "divide ->   -> Num Num   Num   \n divide   num den   = /   num den  "
          expected = Function "divide" (Arrow (Arrow NumType NumType) NumType) ["num","den"] (BinOp Divide (Ident "num") (Ident "den"))
      success result `shouldBe` Just (expected, "")

    it "parse Call with identfier followed by complex list type" $ do
      let result = runp parseCall "(fn x [Num]:[])"
          expected = Call "fn" [Ident "x", Val (List (ListType NumType) [])]
      success result `shouldBe` Just (expected, "")
