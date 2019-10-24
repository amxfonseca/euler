-- Problem 5 - Smallest multiple
module Problem5 where

import Problem1 (isMultipleOf)

isMultipleOfRange :: Int -> [Int] -> Bool
isMultipleOfRange n range =
  null $ dropWhile id $ flip isMultipleOf n <$> range

answer :: Int
answer =
  head [ n | n <- [1..], isMultipleOfRange n $ reverse [2..20]]