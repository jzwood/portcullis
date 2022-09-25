module Util where

import Data.List
import Data.Tuple (swap)
import Data.Map (Map)
import qualified Data.Map as Map

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) f1 f2 a = f1 a && f2 a

(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) f1 f2 a = f1 a || f2 a

-- copied from https://hackage.haskell.org/package/extra-1.7.10/docs/Data-List-Extra.html
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of 0 -> Just x; _ -> r (k-1)) (const Nothing) xs n

-- |
-- >>> paren "cat"
-- "(cat)"
paren :: String -> String
paren a = "(" ++ a ++ ")"

-- |
-- >>> bracket "cat"
-- "[cat]"
bracket :: String -> String
bracket a = "[" ++ a ++ "]"

curly :: String -> String
curly a = "{\n" ++ a ++ "\n}"

pad :: String -> String
pad a = " " ++ a ++ " "

-- |
-- >>> unlines' ["cat", "mouse"]
-- "cat\nmouse"
-- >>> unlines' []
-- ""
unlines' :: [String] -> String
unlines' [] = ""
unlines' xs = init . unlines $ xs

indent :: String -> String
indent = unlines' . fmap ("  "++) . lines

comment :: String -> String
comment = unlines' . map ("// " ++) . lines

multComment :: String -> String
multComment str = concat ["/*\n", unlines . map (" *  " ++) . lines $ str, " */"]

--divider :: String
--divider =  concat $ "// " : replicate 60 "#"

-- |
-- >>> head' 'x' "asdf"
-- 'a'
-- >>> head' 'x' ""
-- 'x'
head' :: a -> [a] -> a
head' x [] = x
head' _ (x:xs) = x

-- |
-- >>> tail' "asdf"
-- "sdf"
-- >>> tail' ""
-- ""
tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

-- |
-- >>> Util.showList ["a", "b"]
-- "[a, b]"
showList :: [String] -> String
showList = bracket . intercalate ", "

uniq :: Ord a => [a] -> Bool
uniq xs = all (==1) $ length <$> (group . sort) xs

-- |
-- >>> dupes [1, 2, 3, 1, 4, 3]
-- [1,3]
-- >>> dupes []
-- []
dupes :: Ord a => [a] -> [a]
dupes xs = head <$> filter ((>1) . length) ((group . sort) xs)

-- |
-- >>> dupesOn id []
-- []
-- >>> dupesOn id [1, 2, 3, 4]
-- []
-- >>> dupesOn id [4, 3, 4, 5, 6, 5]
-- [4,5]
dupesOn :: Ord b => (a -> b) -> [a] -> [a]
dupesOn on xs = head <$> filter ((>1) . length) ((groupBy (\p1 p2 -> on p1 == on p2) . sortOn on) xs)

lookup' :: Ord k => Map k v -> (k -> x) -> k -> Either x v
lookup' m f k =
  case Map.lookup k m of
    Nothing -> Left $ f k
    Just v -> Right v

-- |
-- >>> extractExt "foo.bar"
-- "bar"
-- >>> extractExt "foo"
-- "foo"
extractExt :: String -> String
extractExt = reverse . takeWhile (/= '.') . reverse

invert :: (Ord k, Ord a) => Map k a -> Map a k
invert = Map.fromList . fmap swap . Map.toList
