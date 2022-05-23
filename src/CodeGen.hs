module CodeGen where

import Syntax
import Data.Functor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse, nub)
import Util

instance Show Module where
  show (Module stmts) = unlines
                      $ readAtoms stmts
                      : readZeroArityFunctions stmts
                      : (show <$> stmts)

instance Show Stmt where
  show (Function name tExpr vars expr)
    = comment $ unwords ["function", show name, "has type", show tExpr]
    ++ '\n'
    : concat
    [ def
    , name
    , (paren . head' "") vars
    ]
    ++ unlines'
    [ " {"
    , (indent . concat) [ "return " , concatMap ((++ " => ") . paren) (tail' vars) , show expr , ";" ]
    , "}"
    ]
      where
        def = case vars of
          []  -> "function $"
          _ -> "export function "

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
  show (Arrow tExpr1 tExpr2) = paren (show tExpr1 ++ pad "->" ++ show tExpr2)

instance Show Value where
  show (Number n) = show n
  show (Character c) = ['\'', c, '\'']
  show (Atom n) = n
  show (List t xs) = unwords ["/*", show $ ListType t, "*/", show xs]
  show (Tuple e1 e2)
    =  show <$> [e1, e2]
    &  bracket . intercalate ", "

instance Show Expr where
  show (Val p) = show p
  show (Ident name) = name
  show (Call name exprs) = name ++
    case exprs of
      [] -> ""  -- functions without arguments are interpreted as values
      _ -> concatMap (paren . show) exprs
  show (UnOp unop e) = show e ++ show unop
  show (BinOp Equal e1 e2) = prefixBop Equal e1 e2
  show (BinOp Cons e xs) = bracket $ show e ++ ", ..." ++ show xs
  show (BinOp bop e1 e2) = infixBop bop e1 e2
  show (TernOp If p e1 e2)
    = (paren . ('\n':) . (++"\n") . indent . unlines)
    [ "/* if */ " ++ show p ++ " ?"
    , "/* then */ " ++ show e1 ++ " :"
    , "/* else */ " ++ show e2
    ]
  show (TernOp Uncons xs b (Ident fb)) =
    show $ TernOp If (BinOp Equal xs (Val $ List (Unspecfied "") [])) b (Call fb [UnOp Head xs, UnOp Tail xs])

instance Show UnOp where
  show Fst = "[0]"
  show Snd = "[1]"
  show Head = ".at(0)"
  show Tail = ".slice(1)"

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
  --show Cons

prefixOp :: String -> [String] -> String
prefixOp op = (op ++) . paren . intercalate ", "

prefixBop :: Bop -> Expr -> Expr -> String
prefixBop bop e1 e2 = prefixOp (show bop) (show <$> [e1, e2])

infixBop :: Bop -> Expr -> Expr -> String
infixBop bop e1 e2 = paren . intercalate (pad . show $ bop) $ show <$> [e1, e2]

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
    BinOp _ e1 e2 -> flatFindAtoms [e1, e2]
    TernOp _ e1 e2 e3 -> flatFindAtoms [e1, e2, e3]
    _ -> []

readAtoms :: [Stmt] -> String
readAtoms stmts
  =  concatMap (findAtoms . body) stmts
  &  zip [0..] . nub . ("False" :) . ("True" :)
 <&> (\(i, atom) -> unwords ["const", atom, "=", show i])
  &  unlines

readZeroArityFunctions :: [Stmt] -> String
readZeroArityFunctions stmts
  =  stmts
  &  filter (null . args)
 <&> name
 <&> (\name -> unwords ["export", "const", name, "=", '$' : name ++ "()" ])
  &  unlines
