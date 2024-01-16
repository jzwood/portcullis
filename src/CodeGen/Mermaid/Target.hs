{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Mermaid.Target where

import Data.Function
import Data.Functor
import Syntax
import Util (indent, paren)

-- explore sequence diagram too

class Mermaid ast where
    toMermaid :: ast -> String

instance Mermaid Module where
    toMermaid Module{pipes} = (("flowchart TD\n" ++) . indent) (pipes >>= toMermaid)

instance Mermaid Pipe where
    toMermaid Pipe{funcName, inStreams, outStreamName} =
        inStreams
            <&> (\(name, _) -> arrow (roundLabel name) fname)
            & (arrow fname (roundLabel outStreamName) :)
            & unlines
      where
        roundLabel n = n ++ paren n
        fname = "λ." ++ funcName

arrow :: String -> String -> String
arrow a b = unwords [a, "-->", b]
