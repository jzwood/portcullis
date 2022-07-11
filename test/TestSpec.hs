module TestSpec (spec) where

import Test.Hspec
import Data.Function
import Data.Functor
import Data.Either (isLeft, isRight)
import Data.List
import Syntax
import CodeGen
import Parser
import Typecheck
import qualified Compile as C
import Util
import qualified Data.Map as Map

compile :: String -> Either String String
compile program = mapLeft show (C.compile program)

expectLeft :: String -> String
expectLeft program =
  case compile program of
    Left err -> err
    Right _ -> error "function expected to error did not"

spec :: Spec
spec = do
  describe "sanity checks" $ do

    it "typecheck pipe" $ do
      let badid = [ "badid -> z Num"
                  , "badid x = x"
                  ] & unlines
      expectLeft badid `shouldSatisfy` ("TypeMismatch Num z" `isInfixOf`)
