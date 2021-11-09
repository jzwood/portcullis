module ParserSpec (spec) where

import Test.Hspec
import Parser

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses parens" $ do
      let result = runParser camel mempty "cat"
          expected = Right ("cat",Cursor {line = 0, col = 3},"")
      result `shouldBe` expected
