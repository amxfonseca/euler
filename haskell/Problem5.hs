-- Problem 5 - Smallest multiple
module Problem5 where

import Problem7 (isPrime)

primesLowerThan :: Int -> [Int]
primesLowerThan n =
  takeWhile (< n) [ p | p <- [2..], isPrime p]

-- Lowest multiple of a sequence of numbers from {1...K}
firstMultipleOfK :: Int -> Int
firstMultipleOfK k =
  product $ zipWith (^) primes exponents
  where
    primes = primesLowerThan k
    exponents = floor . flip logBase (fromIntegral k) . fromIntegral <$> primes

answer :: Int
answer =
  firstMultipleOfK 20
