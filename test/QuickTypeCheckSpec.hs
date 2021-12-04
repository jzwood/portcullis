module QuickTypeCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import MiniParser
import Syntax
import Parser


spec :: Spec
spec = do
  describe "addition" $ do
    it "1 + 1" $ do
      property $ \x -> x + 1 > (x :: Int)

