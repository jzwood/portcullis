module Semantics where

import Data.Functor
import Data.Function
import Data.List (nub, foldl')
import Data.Map (Map)
import Syntax

-- ensure function signatures match have same arity in signature --> typechecker will handle this, right?
-- ensure no Atoms are reserved words e.g. Num Char Atom

newtype SemanticError = SemanticError String deriving (Show)

reservedAtoms = ["Num", "Char", "Atom"]
