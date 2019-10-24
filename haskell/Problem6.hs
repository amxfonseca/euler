-- Problem 6 - Sum square difference
module Problem6 where

-- Using a special case of Faulhaber's formula we can deduce that:
-- ∑i = n * (n + 1) / 2
sumOfNaturalNumbers :: Int -> Int
sumOfNaturalNumbers n =
  n * (n + 1) `div` 2

-- Using the same law we can also deduce the following:
-- ∑i² = n * (n + 1) * (2 * n + 1) / 6
sumOfSquares :: Int -> Int
sumOfSquares n =
  n * (n + 1) * (2 * n + 1) `div` 6


answer :: Int
answer =
  (sumOfNaturalNumbers 100) ^ 2 - sumOfSquares 100