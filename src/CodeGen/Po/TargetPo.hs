{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Po.TargetPo where

import Prelude hiding (showList)
import Syntax
import Data.Functor
import Data.Bifunctor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse, nub)
import Util
import Data.Map (Map, (!))
import qualified Data.Map as Map

class Po ast where
  toPo :: ast -> String

instance Po Module where
  toPo Module { functions, comments, streamMap, pipes }
    = unlines $ (toPo <$> functions) ++ [zeroArityFuncs, topology]
      where
        zeroArityFuncs = unlines $ showZeroArityFunctions functions
        topology = showTopology streamMap pipes

instance Po Function where
  toPo (Function name tExpr vars expr)
    = unlines [ unwords [ name, unparen $ toPo tExpr]
              , unwords [name, concat vars, "=", toPo expr]
              ]

instance Po TypeExpr where
  toPo NumType = "Num"
  toPo ByteType = "Byte"
  toPo AtomType = "Atom"
  toPo (Unspecified t) = extractExt t
  toPo (ListType t)
    = toPo t
    & bracket
  toPo (TupType t1 t2)
    =  [t1, t2]
   <&> toPo
    &  bracket . unwords
  toPo (Arrow tExpr1 tExpr2) = paren (toPo tExpr1 ++ pad "->" ++ toPo tExpr2)

instance Po Value where
  toPo (Number n) = show n
  toPo (Byte b) = show b
  toPo (Atom n) = n
  toPo (List (Unspecified "") xs) = showList $ toPo <$> xs -- so uncons displays nicely
  toPo (List t xs) = unwords [toPo $ ListType t, showList $ toPo <$> xs]
  toPo (Tuple e1 e2)
    =  toPo <$> [e1, e2]
    &  showList

instance Po Expr where
  toPo (Val p) = toPo p
  toPo (Ident name) = name
  toPo (Call name exprs) = unwords [ name, concatMap toPo exprs]
  toPo (UnOp unop e) = toPo e ++ toPo unop
  toPo (BinOp bop e1 e2) = toPo $ Call (toPo bop) [e1, e2]
  toPo (TernOp If p e1 e2)
    = (('\n':) . (++"\n") . indent . unwords)
    [ toPo p ++ " ?"
    , toPo e1 ++ " :"
    , toPo e2
    ]
  toPo (TernOp Uncons xs b fb) =
    toPo $ TernOp If (BinOp Equal xs (Val $ List (Unspecified "") [])) b (Call (toPo fb) [UnOp Head xs, UnOp Tail xs])

instance Po UnOp where
  toPo Fst = "@1"
  toPo Snd = "@2"
  toPo Head = "???"
  toPo Tail = "???"

instance Po Bop where
  toPo Plus = "+"
  toPo Minus = "-"
  toPo Times = "*"
  toPo Divide = "/"
  toPo GreaterThan = ">"
  toPo GreaterThanOrEqual = ">="
  toPo LessThan = "<"
  toPo LessThanOrEqual = "<="
  toPo Rem = "%"
  toPo Equal = "=="
  toPo Cons = "+>"

unparen :: String -> String
unparen = init . tail

showZeroArityFunctions :: [Function] -> [String]
showZeroArityFunctions funcs
  =  funcs
  &  filter (null . args)
 <&> name
 <&> (\name -> unwords ["export", "const", name, "=", '$' : name ++ "()" ])

showTopology :: Map Name Stream -> [Pipe] -> String
showTopology _ [] = "export const pipes = [];"
showTopology streamMap pipes
  =  pipes
 <&> showPipe streamMap
  &  ("export const pipes = " ++) . ("[\n" ++) . (++ "\n]") . indent . intercalate ",\n"

showPipe :: Map Name Stream -> Pipe -> String
showPipe streamMap Pipe { funcName, inStreams, outStreamName } =
  showList [funcName, showList (fmap (\(name, buffer) -> showList [show name, show buffer]) inStreams), show outStreamName]
