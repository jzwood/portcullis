module Util where

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) f1 f2 a = f1 a && f2 a

(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) f1 f2 a = f1 a || f2 a

parenthize :: String -> String
parenthize a = "(" ++ a ++ ")"

pad :: String -> String
pad a = " " ++ a ++ " "
