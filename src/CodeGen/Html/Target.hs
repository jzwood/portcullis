{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CodeGen.Html.Target where

import CodeGen.Po.Target (toPo)
import Control.Applicative
import Data.Bifunctor
import qualified Data.ByteString.UTF8 as BSU
import Data.Char
import Data.FileEmbed (embedFile)
import Data.Function
import Data.Functor
import Data.List (dropWhileEnd, groupBy, intercalate, intersperse, nub, sortOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Prelude hiding (showList)

import Syntax
import Util

class Html ast where
    toHtml :: ast -> String

embody :: String -> String
embody content =
    concat
        [ "<!DOCTYPE html>"
        , "<html>"
        , BSU.toString $(embedFile "src/CodeGen/Html/head.html")
        , "<body>"
        , content
        , "</body>"
        , "</html>"
        ]

esc :: String -> String
esc content = content >>= (\case '&' -> "&amp;"; '<' -> "&lt;"; '>' -> "&gt;"; char -> [char])

tag :: String -> [(String, String)] -> String -> String
tag element attrs content =
    concat
        [ "<"
        , unwords [element, unwords $ attrs <&> \(key, val) -> concat [key, "=\"", val, "\""]]
        , ">"
        , content
        , "</"
        , element
        , ">"
        ]

instance Html Module where
    toHtml Module{stmts, functions} =
        (embody . concat)
            [ "<aside>"
            , sortOn name functions >>= \fun -> tag "a" [("href", '#' : name fun)] (name fun)
            , "</aside>"
            , "<main>"
            , unlines' $ toHtml <$> stmts
            , "</main>"
            ]

instance Html Stmt where
    toHtml (F f) = toHtml f
    toHtml (S s) = toHtml s
    toHtml (P p) = toHtml p
    toHtml (C c) = toHtml c

instance Html Function where
    toHtml (Function name tExpr vars expr) =
        tag "code" [("id", name), ("class", "function")] $
            unlines'
                [ unwords [tag "span" [("class", "function-name")] name, toHtml tExpr]
                , unwords [tag "span" [("class", "function-name")] name, unwords $ tag "var" [("class", "param")] <$> vars, "="]
                , indent $ toHtml expr
                ]

instance Html Pipe where
    toHtml pipe = tag "span" [("class", "pipe")] (toPo pipe)

instance Html Stream where
    toHtml Stream{streamName, streamSig} = tag "code" [("class", "stream")] $ unwords [streamName, toHtml streamSig]

instance Html Comment where
    toHtml comment@(Comment _) = tag "p" [("class", "comment")] $ esc . toPo $ comment

instance Html TypeExpr where
    toHtml te = tag "span" [("class", "type")] $ esc . toPo $ te

instance Html Value where
    toHtml num@(Number _) = tag "span" [("class", "val num")] (toPo num)
    toHtml byte@(Byte _) = tag "span" [("class", "val byte")] (toPo byte)
    toHtml atom@(Atom _) = tag "span" [("class", "val atom")] (toPo atom)
    toHtml (List t xs) = concat [toHtml t, ":", (bracket . unwords) $ toHtml <$> xs]
    toHtml (Tuple e1 e2) =
        toHtml <$> [e1, e2]
            & curly . unwords

instance Html Expr where
    toHtml (Val p) = toHtml p
    toHtml (Ident name) = tag "var" [("class", "ident")] name
    toHtml (Call name exprs) = tag "span" [("class", "call")] $ (paren . unwords . filter (not . null)) [tag "span" [("class", "function-name")] name, unwords $ toHtml <$> exprs]
    toHtml (UnOp unop e) = unwords [toHtml unop, toHtml e]
    toHtml (BinOp bop e1 e2) = unwords [toHtml bop, toHtml e1, toHtml e2]
    toHtml (TernOp If p e1 e2) =
        unlines' [unwords [tag "span" [("class", "op")] "?", toHtml p], indent $ toHtml e1, toHtml e2]
    toHtml (TernOp Uncons xs b fb) =
        unwords [tag "span" [("class", "op")] (esc "<+"), toHtml xs, toHtml b, toHtml fb]

instance Html UnOp where
    toHtml op = tag "span" [("class", "op")] (toPo op)

instance Html Bop where
    toHtml op = tag "span" [("class", "op")] (esc . toPo $ op)
