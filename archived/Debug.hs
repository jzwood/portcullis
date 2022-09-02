module Debug where

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


program = "_hof0 -> -> t Atom -> [t] [t] \
          \_hof0 f xs = ? (f xs) xs a [] \
          \n2a -> Num Atom \
          \n2a n = Cat \
          \hof0 [Num] \
          \hof0 = (_hof0 n2a Num [])"

--m = unsafeCompile program
--function = functions m !! 2

x = zip (map (\fxn -> typeofExpr (functionMap m) fxn (body fxn)) (functions m)) (fmap name $ functions m)

bad1 = "bad1 -> -> t Atom -> [t] Num \
       \bad1 f xs = ? (f xs) 1 2"

m = unsafeCompile bad1
function = functions m !! 0
y = zip (map (\fxn -> typeofExpr (functionMap m) fxn (body fxn)) (functions m)) (fmap name $ functions m)



--bad1mod = unsafeCompile bad1
--bad1func = head $ functions bad1mod
--sig = signature bad1func
--bo = body bad1func
--fm = functionMap bad1mod


--hbad1 :: (t -> Bool) -> [t] -> Integer
--hbad1 f xs = if f xs then 1 else 2

main :: IO ()
main = do
  print y
  --print bad1mod
  --print bad1func
  --print sig
  --print bo
  --print fm
