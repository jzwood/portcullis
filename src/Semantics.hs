module Semantics where

import Data.Functor
import Data.Function
import Data.List (foldl')
import Data.Map (Map)
import Syntax

-- ensure that guard ends is correct, ie ends in single default expr
-- ensure each function has a signature
-- ensure function signatures match have same arity in signature
-- ensure no Atoms are reserved words e.g. Num Char Atom
-- Produce Atom table so final generated code contains appropriate mapping e.g. Const False = 0

newtype SemanticError = SemanticError String deriving (Show)

type SignatureMap = Map String TypeExpr
type FunctionMap = Map String ([Var], Expr)

stmtToName :: Statement -> String
stmtToName (Signature name _) = name
stmtToName (Function name _ _) = name

stmtToMapAlg :: (SignatureMap, FunctionMap) -> Statement -> Either SemanticError (SignatureMap, FunctionMap)
stmtToMapAlg (sigMap, funcMap) sig@(Signature name texp) =
  if name `member` sigMap
  then Left $ SemanticError $ "Signature name already exists"
  else (Map.insert name texp, funcMap)
stmtToMapAlg (sigMap, funcMap) func@(Function name args body) =
  if name `member` funcMap
  then Left $ SemanticError $ "Function name already exists"
  else (sigMap, Map.insert name (args, body))

mergeMaps :: (SignatureMap, FunctionMap) ->

modToStatements :: Module -> Either SemanticError [Statement]
modToStatements (Module _ stmts) = do
  (sigMap, funMap) <- foldl' stmtToMapAlg (Right (Map.empty, Map.empty)) stmts

  =  stmts
 <&> stmtToName
 <&>



-- =
-- & flip zip (singleton <$> stms)
-- & Map.fromListWith (++)

    --isFun (Function _ _ _) = True
    --isFun _ = False
    --isSig (Signature _ _) = True
    --isSig _ = False
    --functions = map () filter isFun stmts

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
    Guard tes -> concatMap (\(e1, e2) -> flatFindAtoms [e1, e2]) tes
    BinOp _ e1 e2 -> flatFindAtoms [e1, e2]
    TernOp _ e1 e2 e3 -> flatFindAtoms [e1, e2, e3]
    _ -> []

  {-
mapAtoms :: Module -> String
mapAtoms mod
  =  (modToStatements mod)
 <&> findAtoms . body
  &  zip [0..] . nub . ("False" :) . concat
 <&> (\(i, atom) -> unwords ["const", atom, "=", show i])
  &  unlines
-}
