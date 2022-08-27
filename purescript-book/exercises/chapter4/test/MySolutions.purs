module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (cons, filter, head, length, null, tail, (..))
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Test.Examples (factorsV3)

isEven :: Int -> Boolean
isEven n =
  if n == 2 || n == 0 then
    true
  else if n == 1 then
    false
  else
    if n < 0 then
      isEven $ n + 2
    else
      isEven $ n - 2

countEven :: Array Int -> Int
countEven xs =
  if null xs then
    0
  else
    oneIfEven (fromMaybe 1 $ head xs) + countEven (fromMaybe [] $ tail xs)

  where
    oneIfEven n = if mod n 2 == 0 then 1 else 0

squared :: Array Number -> Array Number
squared = map (\x -> x * x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\x -> x >= 0.0)

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs = (\x -> x >= 0.0) <$?> xs

isPrime :: Int -> Boolean
isPrime n = n /= 1 && (length $ factorsV3 n) == 1

cartesianProduct :: forall (a :: Type ). Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  a <- (1 .. n)
  b <- (a .. n)
  c <- (b .. n)
  guard $ (a*a + b*b) == c*c
  pure [a, b, c]

primeFactors :: Int -> Array Int
primeFactors n =
  if n == 1 then
    []
  else
    cons firstPrimeFactor $ primeFactors (n / firstPrimeFactor)

  where
    firstPrimeFactor :: Int
    firstPrimeFactor =
      fromMaybe n <<< head $ filter (\x -> n `mod` x == 0) (2 .. n)

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\a b -> a && b) true

fibTailRec :: Int -> Int
fibTailRec n = fib' n 0 1
    where
      fib' n' a b =
        if n' == 0 then
          a
        else if n' == 1 then
          b
        else
          fib' (n' - 1) b (a + b)
          
reverse :: forall (a :: Type). Array a -> Array a
reverse = foldl (\xs x -> cons x xs) []
