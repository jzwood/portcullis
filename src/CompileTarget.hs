module CompileTarget where

import Syntax
import Data.Functor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, nub)
import Util

instance Show Module where
  show (Module stmts) = unlines $ (readAtoms stmts) : (show <$> stmts)

instance Show Stmt where
  show (Function name tExpr vars expr)
    = comment $ unwords ["function", show name, "has type", show tExpr]
    ++ '\n'
    : concat
    [
    "export function "
    , name
    , (paren . head' "") vars
    ]
    ++ unlines'
    [ " {"
    , (indent . concat) [ "return " , concatMap ((++ " => ") . paren) (tail' vars) , show expr , ";" ]
    , "}"
    ]

instance Show TypeExpr where
  show NumType = "Num"
  show CharType = "Char"
  show AtomType = "Atom"
  show (Unspecfied t) = t
  show (ListType t)
    = show t
    & bracket
  show (TupType t1 t2)
    =  [t1, t2]
   <&> show
    &  bracket . unwords
  show (Arrow tExpr1 tExpr2) = paren (show tExpr1 ++ (pad "->") ++ show tExpr2)

instance Show Value where
  show (Number n) = show n
  show (Character c) = '\'' : c : '\'' : []
  show (Atom n) = n
  show (List t a) = unwords ["/*", show $ ListType t, "*/", show a]
  show (Tuple e1 e2)
    =  show <$> [e1, e2]
    &  bracket . (intercalate ",")

instance Show Expr where
  show (Val p) = show p
  show (Ident name) = name
  show (Call name exprs) = name ++
    case exprs of
      [] -> "()"
      _ -> concatMap (paren . show) exprs
  show (UnOp unop e) = show unop ++ (paren $ show e)
  show (BinOp Equal e1 e2) = prefixBop Equal e1 e2
  show (BinOp Concat a1 a2) = prefixBop Concat a1 a2
  show (BinOp bop e1 e2) = infixBop bop e1 e2
  show (Guard cases defaultCase) = showGuard cases defaultCase
  show (TernOp At a n e) = paren $ prefixOp (show At) (show <$> [a, n]) ++ " ?? " ++ show n
  show (TernOp top e1 e2 e3) = prefixTop top e1 e2 e3

instance Show UnOp where
  show Fst = "(([a,]) => a)"
  show Snd = "(([,b]) => b)"
  show Length = "((arr) => arr.length)"

instance Show Bop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show Rem = "%"
  show Equal = "equal"
  show Concat = "Array.prototype.concat.call"

instance Show Top where
  show Slice = "Array.prototype.slice.call"
  show At = "Array.prototype.at.call"


showGuard :: [(Expr, Expr)] -> Expr -> String
showGuard cases defaultCase =
  unlines' [ "(() => {"
          , (indent . unlines') (concatMap showCase cases ++ [showReturn defaultCase])
          , "})()"
          ]
  where
    showReturn :: Expr -> String
    showReturn expr = "return " ++ show expr ++ ";"
    showCase :: (Expr, Expr) -> [String]
    showCase (predicate, expr) =
      [ "if (" ++ show predicate ++ ") {"
      , '\t' : showReturn expr
      , "}"
      ]

prefixOp :: String -> [String] -> String
prefixOp op = (op ++) . paren . (intercalate ", ")

prefixBop :: Bop -> Expr -> Expr -> String
prefixBop bop e1 e2 = prefixOp (show bop) (show <$> [e1, e2])

prefixTop :: Top -> Expr -> Expr -> Expr -> String
prefixTop top e1 e2 e3 = prefixOp (show top) (show <$> [e1, e2, e3])

infixBop :: Bop -> Expr -> Expr -> String
infixBop bop e1 e2 = paren . (intercalate $ show bop) $ show <$> [e1, e2]

-- someday we might refactor AST so that we can fold it into list of Atoms
findAtoms :: Expr -> [String]
findAtoms expr =
  let
    flatFindAtoms :: [Expr] -> [String]
    flatFindAtoms = concatMap findAtoms
  in case expr of
    Val v ->
      case v of
        Atom n -> [n]
        Tuple e1 e2 -> flatFindAtoms [e1, e2]
        List _ es -> flatFindAtoms es
        _ -> []
    Call n [es] -> flatFindAtoms [es]
    Guard cases defCase -> flatFindAtoms $ defCase : concatMap (\(e1, e2) -> [e1, e2]) cases
    BinOp _ e1 e2 -> flatFindAtoms [e1, e2]
    TernOp _ e1 e2 e3 -> flatFindAtoms [e1, e2, e3]
    _ -> []

readAtoms :: [Stmt] -> String
readAtoms stmts
  =  concatMap (findAtoms . body) stmts
  &  zip [0..] . nub . ("False" :)
 <&> (\(i, atom) -> unwords ["const", atom, "=", show i])
  &  unlines
