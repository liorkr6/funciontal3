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
  prettyPrint (Literal False) = "false"
  prettyPrint (Literal True) = "true"
  prettyPrint (Var x) = x
  prettyPrint (Not exp) = "!" ++ case exp of
    Literal _ -> prettyPrint exp
    Var _ -> prettyPrint exp
    _ -> "(" ++ prettyPrint exp ++ ")"
  prettyPrint (And exp1 exp2) = prettyPrint exp1 ++ " && " ++ prettyPrint exp2
  prettyPrint (Or exp1 exp2) = prettyPrint exp1 ++ " || " ++ prettyPrint exp2

identation :: Int -> String
identation level = replicate (level * 2) ' '

prettyPrintHelperIdentation :: Int -> Statement -> String
prettyPrintHelperIdentation level (Return exp) = identation level ++ "return " ++ prettyPrint exp
prettyPrintHelperIdentation level (Block s) = identation level ++ "{\n" ++ unlines (map (prettyPrintHelperIdentation (level + 1)) s) ++ identation level ++ "}"
prettyPrintHelperIdentation level (If exp s) = identation level  ++ "If (" ++ prettyPrint exp ++ ") {\n" ++ unlines (map (prettyPrintHelperIdentation (level + 1)) s) ++ identation level ++ "}"
prettyPrintHelperIdentation level (IfElse exp s1 s2) = prettyPrintHelperIdentation level (If exp s1) ++ " else {\n" ++  unlines (map (prettyPrintHelperIdentation (level + 1)) s2) ++ identation level ++ "}"
prettyPrintHelperIdentation level (Define var exp) = identation level ++ var ++ " = " ++ prettyPrint exp

instance PrettyPrint Statement where
  prettyPrint = prettyPrintHelperIdentation 0

instance PrettyPrint [Statement] where
  prettyPrint = unlines . map prettyPrint

-- -- Section 1.2: Simplifying expressions and statements
type Scope = Map Variable Bool

simplifyExpression :: Scope -> Expression -> Expression
simplifyExpression _ (Literal x) = Literal x
simplifyExpression scope (Not exp) =
  case simplifyExpression scope exp of
    Literal val -> Literal (not val)
    simplifiedExp -> Not simplifiedExp
simplifyExpression scope (Or exp1 exp2) =
  case (simplifyExpression scope exp1, simplifyExpression scope exp2) of
    (Literal True, _) -> Literal True
    (_, Literal True) -> Literal True
    (Literal val1, Literal val2) -> Literal (val1 || val2)
    (simplifiedExp1, simplifiedExp2) -> Or simplifiedExp1 simplifiedExp2
simplifyExpression scope (And exp1 exp2) =
  case (simplifyExpression scope exp1, simplifyExpression scope exp2) of
    (Literal False, _) -> Literal False
    (_, Literal False) -> Literal False
    (Literal val1, Literal val2) -> Literal (val1 && val2)
    (simplifiedExp1, simplifiedExp2) -> And simplifiedExp1 simplifiedExp2
simplifyExpression scope (Var x) =
  case M.lookup x scope of
    Just val -> Literal val
    Nothing -> Var x

simplifyWithScope :: Scope -> [Statement] -> [Statement]
simplifyWithScope s = reverse . snd . foldl' (uncurry go) (s, []) where
  go :: Scope -> [Statement] -> Statement -> (Scope, [Statement])
  go scope statementsSoFar statement =
    let (newScope, simplified) = simplifyStatement scope statement
     in (newScope, simplified ++ statementsSoFar)
  simplifyStatement :: Scope -> Statement -> (Scope, [Statement])
  simplifyStatement scope (Return exp) = (scope, [Return $ simplifyExpression scope exp])
  simplifyStatement scope (Block states) = (scope, [Block $ simplifyWithScope scope states])
  simplifyStatement scope (Define var exp) = case simplifyExpression scope exp of
    Literal bool_val -> (scope', [Define var (Literal bool_val)]) where scope' = M.insert var bool_val scope
    _ -> (scope', [Define var $ simplifyExpression scope exp]) where scope' = M.delete var scope
  simplifyStatement scope (If exp states) = case simplifyExpression scope exp of
    Literal False -> (scope, [])
    Literal True -> simplifyStatement scope (Block states)
    Var x -> let scope' = M.insert x True scope
              in (scope', [If exp $ simplifyWithScope scope' states])
    _ -> (scope, [If exp $ simplifyWithScope scope states])
  simplifyStatement scope (IfElse exp states1 states2) = case simplifyExpression scope exp of
    Literal True -> (scope, simplifyWithScope scope states1)
    Literal False -> (scope, simplifyWithScope scope states2)
    Var x -> let updateVar b = M.insert x b scope
              in (scope, [IfElse (Var x) (simplifyWithScope (updateVar True) states1) (simplifyWithScope (updateVar False) states2)])
    _ -> (scope, [IfElse exp (simplifyWithScope scope states1) (simplifyWithScope scope states2)])



simplify :: [Statement] -> [Statement]
simplify = simplifyWithScope M.empty  

-- Section 2.1: Basic type classes
data Tree a = Empty | Tree (Tree a) a (Tree a)
instance Show a => Show (Tree a) where
  show x = "{" ++ init (go x) ++ "}" where
    go :: Tree a  -> String
    go = \case
      Empty -> ""
      (Tree left value right) -> go left ++ show value ++ "," ++ go right

treeToList:: Tree a -> [a]
treeToList Empty = []
treeToList (Tree left val right) = treeToList left ++ [val] ++ treeToList right

instance Eq a => Eq (Tree a) where
  Empty == Empty = True
  (Tree left1 val1 right1) == (Tree left2 val2 right2) = treeToList (Tree left1 val1 right1) == treeToList (Tree left2 val2 right2)
  _ == _ = False

instance Ord a => Ord (Tree a) where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Tree left1 val1 right1) (Tree left2 val2 right2) =  treeToList (Tree left1 val1 right1) `compare` treeToList (Tree left2 val2 right2)

-- -- Section 2.2: Typeclass constraints
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)
  where
    insert :: Ord a => a -> [a] -> [a]
    insert z [] = [z]
    insert z (y:ys) = case z `compare` y of
      LT  -> z : y : ys
      EQ -> z : y : ys
      _ -> y : insert z ys

sortOn :: forall a b. Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn f (x:xs) = insert x (sortOn f xs)
  where
    insert :: Ord b => a -> [a] -> [a]
    insert z [] = [z]
    insert z (y:ys) = case f z `compare` f y of
      LT -> x : y : ys
      EQ -> x : y : ys
      GT -> y : insert x ys