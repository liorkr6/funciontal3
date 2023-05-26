{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Needed for instance PrettyPrint [Statement]
{-# LANGUAGE FlexibleInstances #-}

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
-- {-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import qualified Data.Map as M
import Data.Map (Map, (!?))
import qualified Data.Set as S
import Data.Set (Set)
import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Ordering(..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, not, notElem, null, or, product, replicate, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unlines, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

type Variable = String
data Expression = Not Expression | Or Expression Expression | And Expression Expression | Var Variable | Literal Bool deriving (Show, Eq)
data Statement =
  Return Expression |
  Block [Statement] |
  If Expression [Statement] |
  IfElse Expression [Statement] [Statement] |
  Define Variable Expression
  deriving (Show, Eq)

-- Section 1.1: Pretty printing expressions
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Expression where
instance PrettyPrint Statement where
instance PrettyPrint [Statement] where

-- Section 1.2: Simplifying expressions and statements
type Scope = Map Variable Bool

simplifyExpression :: Scope -> Expression -> Expression

simplifyWithScope :: Scope -> [Statement] -> [Statement]
simplifyWithScope s = reverse . snd . foldl' (uncurry go) (s, []) where
  go :: Scope -> [Statement] -> Statement -> (Scope, [Statement])
  go scope statementsSoFar statement =
    let (newScope, simplified) = simplifyStatement scope statement
     in (newScope, simplified ++ statementsSoFar)
  simplifyStatement :: Scope -> Statement -> (Scope, [Statement])

simplify :: [Statement] -> [Statement]

-- Section 2.1: Basic type classes
data Tree a = Empty | Tree (Tree a) a (Tree a)
instance Show a => Show (Tree a) where
instance Eq a => Eq (Tree a) where
instance Ord a => Ord (Tree a) where

-- Section 2.2: Typeclass constraints
nub :: Eq a => [a] -> [a]
sort :: Ord a => [a] -> [a]
sortOn :: Ord b => (a -> b) -> [a] -> [a]
