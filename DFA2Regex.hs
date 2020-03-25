{-|
Module : DFA2Regex
Description : Contains all relevant methods
Copyright : Simón Fallon, Mateo Restrepo
This module contains the main function DFA2Regex and all the
other methods that were implemented in order to convert a DFA
to its equivalent Regular Expression.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module  DFA2Regex where
import DFA
import Regex
import Data.Set as Set
import Data.List as List
import Data.Map(Map)
import qualified Data.Map as Map

{-|
 The 'dfa2Regex' function returns the equivalent regular expression of a DFA.
 It takes one argument of type DFA.
-}
dfa2Regex :: Ord s => DFA s c -> Regex c
dfa2Regex a = multiple (Set.elems (accepting a)) (Set.size(states a)) a

{-|
 This function merges two arrays, the code was taken from:
https://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell
-}
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

{-|
 This function takes a DFA and returns a set containing all its states
 -}
states:: Ord s => DFA s c -> Set s
states dfa = Set.fromList (array (Map.assocs (delta dfa)))
  where
    array :: [(s, Map c s)] -> [s]
    array
        = List.foldr (\ x -> merge (merge [fst x] (Map.elems (snd x)))) []

{-|
  This auxiliar function helps to calculate the regular expession in the case
  that it has multiple transitions from 'i' to 'j'.
    -}
aux1 :: Eq s => Ord s => s -> [(c, s)] -> Regex c
aux1 _ [] = Empty
aux1 j (x:xs) = if snd x == j
                    then Plus (Symbol (fst x)) (aux1 j xs)
                    else aux1 j xs
{-|
  This function takes two states (i,j) and the transition function of a DFA and
  returns the regular expression that represents the paths from i to j.
    -}
aux :: Eq s => Ord s => (s, s) -> Map s (Map c s) -> Regex c
aux (i,j) delt
  |Map.member i delt = aux1 j  (Map.assocs (delt Map.! i))
  |otherwise = Empty

-- |  This function basically applies the transitive closure
rec :: forall s c. Eq s => Ord s => Int -> (s, s) -> DFA s c-> Regex c
rec k (i, j) dfa
  | k == -1 && i == j = Plus Epsilon (aux (i, j) (delta dfa))
  | k == -1 = aux (i, j) (delta dfa)
  | otherwise = let n :: s
                    n = Set.toList (states dfa) List.!! k
                    r1 :: Regex c
                    r1 = simplify (rec (k - 1) (i, j) dfa)
                    r2:: Regex c
                    r2 = simplify (rec (k - 1) (i, n) dfa)
                    r3 :: Regex c
                    r3 = simplify (Star (rec (k - 1) (n , n) dfa))
                    r4 :: Regex c
                    r4 = simplify (rec (k - 1) (n, j) dfa)
                in simplify (Plus r1 (Dot r2(Dot r3 r4)))

{-|
  This Function applies the transitive closure for multiple
  accepting states
-}
multiple :: Eq s => Ord s => [s] -> Int -> DFA s c-> Regex c
multiple xs k dfa
    = List.foldr (\ x -> Plus (rec (k - 1) (start dfa, x) dfa)) Empty xs

{-|
  This function uses some properties in order to  simplify a given
  regular expression or a specific symbol
-}
simplify :: Regex c -> Regex c
simplify Empty = Empty
simplify Epsilon = Epsilon
simplify (Symbol a) = Symbol a
simplify (Plus Epsilon Epsilon) = Epsilon
simplify (Star Empty) = Epsilon
simplify (Star Epsilon) = Epsilon
simplify (Star (Plus Epsilon a)) = Star (simplify a)
simplify (Star(Star a)) = Star (simplify a)
simplify (Star a) = Star (simplify a)
simplify (Dot a Empty) = Empty
simplify (Dot Empty a) = Empty
simplify (Dot a Epsilon) = simplify a
simplify (Dot Epsilon a) = simplify a
simplify (Dot a b) = Dot(simplify a)(simplify b)
simplify (Plus a Empty) = simplify a
simplify (Plus Empty a) = simplify a
simplify (Plus a b) = Plus(simplify a)(simplify b)