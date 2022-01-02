module Semantics where

import Data.Functor
import Data.Function
import Data.List (nub, foldl')
import Data.Map (Map)
import Syntax

-- ensure that guard ends is correct, ie ends in single default expr -> this can be moved to Syntax
-- ensure function signatures match have same arity in signature --> typechecker will handle this, right?
-- ensure no Atoms are reserved words e.g. Num Char Atom
-- Produce Atom table so final generated code contains appropriate mapping e.g. Const False = 0

newtype SemanticError = SemanticError String deriving (Show)

reservedAtoms = ["Num", "Char", "Atom"]
