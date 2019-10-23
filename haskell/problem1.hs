-- Problem 1 - Multiples of 3 and 5
module Problem1 where

isMultipleOf :: Int -> Int -> Bool
isMultipleOf divisor value =
  mod value divisor == 0

multiplesOf :: Int -> Int -> [Int]
multiplesOf num limit =
  [ n | n <- [1..limit], isMultipleOf num n ]

answer :: Int
answer =
  sum (mult 3) + sum (mult 5) - sum (mult 15)
  where
    mult n = multiplesOf n 999
