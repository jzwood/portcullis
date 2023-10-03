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
import Prelude hiding (showList, span)

import Syntax
import Util

class Html ast where
    toHtml :: ast -> String

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

span :: [(String, String)] -> String -> String
span = tag "span"

instance Html Module where
    toHtml Module{stmts, functions, pipes, streams} =
        concat
            [ "<!DOCTYPE html>"
            , BSU.toString $(embedFile "src/CodeGen/Html/head.html")
            , tag "html" [] $
                tag "body" [] $
                    tag "main" [] $ aside ++ section
            ]
      where
        aside = tag "aside" [] $
            unlines [ tag "h3" [] "functions"
                    , sortOn name functions >>= \fun -> tag "a" [("href", '#' : name fun)] (name fun)
                    , tag "h3" [] "pipes"
                    , sortOn funcName pipes >>= \pipe -> tag "a" [("href", '#' : funcName pipe)] (funcName pipe)
                    , tag "h3" [] "streams"
                    , sortOn streamName streams >>= \stream -> tag "a" [("href", '#' : streamName stream)] (streamName stream)
                    ]
        section = tag "section" [] $ unlines' $ toHtml <$> stmts

instance Html Stmt where
    toHtml (F f) = toHtml f
    toHtml (S s) = toHtml s
    toHtml (P p) = toHtml p
    toHtml (C c) = toHtml c

instance Html Function where
    toHtml (Function name tExpr vars expr) =
        tag "code" [("id", name), ("class", "function")] $
            unlines'
                [ unwords [span [("class", "function-name")] name, toHtml tExpr]
                , unwords' [span [("class", "function-name")] name, unwords $ span [("class", "param")] <$> vars, "="]
                , indent $ toHtml expr
                ]

instance Html Pipe where
    toHtml Pipe{funcName, inStreams, outStreamName} =
        tag "code" [("id", funcName), ("class", "pipe")] $ unwords ["<strong>|</strong>", span [("class", "pipe-name")] funcName, (bracket . pad . unwords) $ (\(a, b) -> unwords [span [("class", "function-name")] a, span [("class", "val")] $ show b]) <$> inStreams, span [("class", "function-name")] outStreamName]

instance Html Stream where
    toHtml Stream{streamName, streamSig} = tag "code" [("id", streamName), ("class", "stream")] $ unwords [span [("class", "stream-name")] streamName, toHtml streamSig]

instance Html Comment where
    toHtml comment@(Comment _) = tag "p" [("class", "comment")] $ esc . toPo $ comment

instance Html TypeExpr where
    toHtml te = span [("class", "type")] $ esc . toPo $ te

instance Html Value where
    toHtml num@(Number _) = span [("class", "val num")] (toPo num)
    toHtml byte@(Byte _) = span [("class", "val byte")] (toPo byte)
    toHtml atom@(Atom _) = span [("class", "val atom")] (toPo atom)
    toHtml (List t xs) = concat [toHtml t, ":", (bracket . unwords) $ toHtml <$> xs]
    toHtml (Tuple e1 e2) =
        toHtml <$> [e1, e2]
            & curly . unwords

instance Html Expr where
    toHtml (Val p) = toHtml p
    toHtml (Ident name) = span [("class", "ident")] name
    toHtml (Call name exprs) = span [("class", "call")] $ (paren . unwords') [span [("class", "function-name")] name, unwords $ toHtml <$> exprs]
    toHtml (UnOp unop e) = unwords [toHtml unop, toHtml e]
    toHtml (BinOp bop e1 e2) = unwords [toHtml bop, toHtml e1, toHtml e2]
    toHtml (TernOp If p e1 e2) =
        unlines' [unwords [span [("class", "op")] "?", toHtml p], indent $ toHtml e1, toHtml e2]
    toHtml (TernOp Uncons xs b fb) =
        unwords [span [("class", "op")] (esc "<+"), toHtml xs, toHtml b, toHtml fb]

instance Html UnOp where
    toHtml op = span [("class", "op")] (toPo op)

instance Html Bop where
    toHtml op = span [("class", "op")] (esc . toPo $ op)
