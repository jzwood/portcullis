module Util where

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) f1 f2 a = f1 a && f2 a

(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) f1 f2 a = f1 a || f2 a

paren :: String -> String
paren a = "(" ++ a ++ ")"

bracket :: String -> String
bracket a = "[" ++ a ++ "]"

pad :: String -> String
pad a = " " ++ a ++ " "

unlines' :: [String] -> String
unlines' = init . unlines

indent :: String -> String
indent = unlines' . fmap ('\t' :) . lines

--indent :: String -> String
--indent str = concatMap (\c -> if c == '\n' then "\n\t" else [c]) str

comment :: String -> String
comment = ("// " ++)

multComment :: String -> String
multComment str = concat ["/*\n", unlines . map (" *  " ++) . lines $ str, " */"]

divider :: String
divider =  concat $ "// " : replicate 60 "#"
