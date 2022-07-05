module Util where

import Data.List

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) f1 f2 a = f1 a && f2 a

(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) f1 f2 a = f1 a || f2 a

-- copied from https://hackage.haskell.org/package/extra-1.7.10/docs/Data-List-Extra.html
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

paren :: String -> String
paren a = "(" ++ a ++ ")"

bracket :: String -> String
bracket a = "[" ++ a ++ "]"

curly :: String -> String
curly a = "{\n" ++ a ++ "\n}"

pad :: String -> String
pad a = " " ++ a ++ " "

unlines' :: [String] -> String
unlines' [] = ""
unlines' xs = init . unlines $ xs

indent :: String -> String
indent = unlines' . fmap ("  "++) . lines

comment :: String -> String
comment = unlines' . map ("// " ++) . lines

multComment :: String -> String
multComment str = concat ["/*\n", unlines . map (" *  " ++) . lines $ str, " */"]

divider :: String
divider =  concat $ "// " : replicate 60 "#"

head' :: a -> [a] -> a
head' x [] = x
head' _ (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

showList :: [String] -> String
showList = bracket . intercalate ", "

uniq :: Ord a => [a] -> Bool
uniq xs = all (==1) $ length <$> (group . sort) xs

dupes :: Ord a => [a] -> [a]
dupes xs = head <$> filter ((>1) . length) $ (group . sort) xs

dupesOn :: Ord b => [a] -> (a -> b) -> [a]
dupesOn xs on = head <$> filter ((>1) . length) $ (groupBy (\p1 p2 -> on p1 == on p2) . sortOn on) xs
