module Transpile where


data CompileError = CompileError deriving (Show, Eq)

transpile :: String -> Either CompileError String
transpile = undefined
