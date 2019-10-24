-- Problem 3 - Largest prime factor
module Problem3 where

largestPrimeFactor_ :: Int -> Int -> Int
largestPrimeFactor_ n fact
  | fact >= n = n
  | modN == 0 = largestPrimeFactor_ quotient fact
  | otherwise = largestPrimeFactor_ n (succ fact)
  where
    (quotient, modN) = divMod n fact

largestPrimeFactor :: Int -> Int
largestPrimeFactor n = largestPrimeFactor_ n 2

answer :: Int
answer = largestPrimeFactor 600851475143
