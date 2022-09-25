module Debug where

import Data.Function
import Data.Functor
import Data.Map (Map)
import Control.Applicative
import qualified Data.Map as Map

-- TODO okay what I think I really have to do, unfortately, is step through bad1 and really think through what state I would keep track of (e.g. the type map) and see if our code truly matches those beats (hint: it isn't)

import Syntax
import qualified Data.Set as Set
import Typecheck hiding (TypecheckError)
import qualified Typecheck as T
import qualified Compile as C
import Data.List

unsafeCompile :: String -> Module
unsafeCompile program =
  case C.parse program of
    Right mod -> mod
    Left _ -> error "program unexpectedly failed to parse"


bad1 = "id3 -> z -> [z] [z] \
       \id3 x xs = xs \
       \tail -> [t] [t] \
       \tail xs = <+ xs xs (id3)"

mo = unsafeCompile bad1
b = functions mo !! 0
b1 = functions mo !! 1

m = Map.fromList [("id3", b), ("tail", b1)]

--bad1mod = unsafeCompile bad1
--bad1func = head $ functions bad1mod
--sig = signature bad1func
bo = body b
--fm = functionMap bad1mod


y = zip (map (\fxn -> typeofExpr (functionMap mo) fxn (body fxn)) (functions mo)) (fmap name $ functions mo)

check :: Function -> Map Name Function -> [Expr] -> TypeExpr -> Either TypeError TypeExpr
check f m es sig = mapM (typeofExpr m f) es
  >>= foldl' (\ms t -> ms >>= \(m, s) -> typecheckExpr m t s) (Right (Map.empty, sig))
  <&> snd

check2 f m es sig = mapM (typeofExpr m f) es

res = check b1 m [Ident "xs", Ident "xs", Call "id3" []] (typeofTop Uncons)
res2 = check2 b1 m [Ident "xs", Ident "xs", Call "id3" []] (typeofTop Uncons)

maybeSignature :: Map Name Function -> [Name] -> TypeExpr -> String -> Maybe TypeExpr
maybeSignature m args sig name = argToMaybeSig name args sig <|> (signature <$> Map.lookup name m)

ms = maybeSignature m (args b1) (signature b1) "id3"

--typeofExpr :: Map Name Function -> Function -> Expr -> Either TypeError TypeExpr
sig = signature b
toe = typeofExpr m b1 (body b1)
--res = check b1 m [body b1] (Arrow (Unspecified "a") (Unspecified "a"))


--hbad1 :: (t -> Bool) -> [t] -> Integer
--hbad1 f xs = if f xs then 1 else 2

--main :: IO ()
--main = do
  --print bad1mod
  --print bad1func
  --print sig
  --print bo
  --print fm
