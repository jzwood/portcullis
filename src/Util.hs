module Util where

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) f1 f2 a = f1 a && f2 a

(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) f1 f2 a = f1 a || f2 a

parenthize :: String -> String
parenthize a = "(" ++ a ++ ")"

pad :: String -> String
pad a = " " ++ a ++ " "

indent :: String -> String
indent = unlines . map ('\t' :) . lines

comment :: String -> String
comment = ("// " ++)

multComment :: String -> String
multComment str = concat ["/*\n", unlines . map (" *  " ++) . lines $ str, " */"]
