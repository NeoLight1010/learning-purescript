module Test.MySolutions where

import Prelude

import Data.Array (head, null, tail)
import Data.Maybe (fromMaybe)

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
