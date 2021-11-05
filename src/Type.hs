module Type where

import Parser
import Data.Map (Map)
import Control.Applicative
import Data.Char hiding (chr)
import qualified Data.Map as Map


--primitives: number
--primitives: character (char)
--type: atom primitive*
--type: type | type


--type signature EBNF:

--Primitive := Number | Char
--Type := (Primitive | Atom {Primitive}) [Or Type]

-- Or, Number, Char are terminal

type Name = String
data Primitive = A Name | C Char | N Double -- | char tree | num tree
  deriving (Show, Eq)

data Sum a = Or' a | Or a (Sum a)
  deriving (Show, Eq)

data Product a = And' a | And a (Product a)
  deriving (Show, Eq)

data Atom a = Atom Name a
  deriving (Show, Eq)

data Prim = Character | Number
  deriving (Show, Eq)

data Type = ADT (Sum (Atom (Product Prim)))
  deriving (Show, Eq)

data Pattern
  = PVar Name [Name]-- Mu a b 3
  -- | PCon Name  -- C x y
   | PWild -- _
  deriving (Show, Eq)

  {--
     type Cat = Bronte | Zeta
     type Hello = House Cat
    --}

--data TypeSig = Atom (Product Prim)

-- ADT (Or (Atom "Var" (And (N 34.0) NilAnd)) NilOr)
-- expressions:
-- type Vector = Vector N N N
-- Vector 34 54 1
-- type Money = Yen N | USD N | Euro N
-- Yen 34.123 | USD 12.3 | Euro 0.12 | Zilch

--primitiveT :: Parser Primitive
--primitiveT = N <$> number <|> C <$> chr

--toProduct :: [Primitive] -> Product Primitive
--toProduct [] = NilAnd
--toProduct (p:ps) = And p (toProduct ps)

--productT :: Parser (Product Primitive)
--productT = toProduct <$> (zeroOrMore (spaces *> primitiveT))

--atomT :: Parser (Atom (Product Primitive))
--atomT = liftA2 Atom ident productT

--parseAtomInteger :: Parser Atom
--parseAtomInteger = (N . readInt) <$> parseInteger

--parseAtomIdent :: Parser Atom
--parseAtomIdent = I <$> ident

--parseAtom :: Parser Atom
--parseAtom = parseAtomInteger <|> parseAtomIdent
