{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Mermaid.Target where

import Control.Applicative
import Data.Bifunctor
import Data.Char
import Data.Function
import Data.Functor
import Data.Map (Map)
import Syntax
import Util

class Mermaid ast where
    toMermaid :: ast -> String

instance Mermaid Module where
    toMermaid Module{streamMap, pipes} = showTopology streamMap pipes

showTopology :: Map Name Stream -> [Pipe] -> String
showTopology _ [] = ""
showTopology streamMap pipes =
    (("stateDiagram-v2\n" ++) . indent) (pipes >>= showPipe streamMap)

showPipe :: Map Name Stream -> Pipe -> String
showPipe streamMap Pipe{funcName, inStreams, outStreamName} =
    inStreams
        <&> (\(name, buffer) -> unwords [name, "-->", outStreamName, ":", funcName])
        & unlines
