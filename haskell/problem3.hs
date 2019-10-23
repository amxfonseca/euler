-- Problem 3 - Largest prime factor
module Problem3 where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime_ :: Int -> Int -> Bool
isPrime_ num divisor
    | divisor < 2 = True
    | mod num divisor == 0 = False
    | otherwise = isPrime_ num (pred divisor)

isPrime :: Int -> Bool
isPrime n = isPrime_ n (isqrt n)

nextPrime :: Int -> Int
nextPrime n =
  if isPrime next then
    next
  else
    nextPrime next
  where
    next = succ n

largestPrimeFactor_ :: Int -> Int -> Int
largestPrimeFactor_ n factor =
  if modN == 0 then
    if isPrime n then
      n
    else
      largestPrimeFactor_ quotient factor
  else
    largestPrimeFactor_ n (nextPrime factor)
  where
    (quotient, modN) = divMod n factor

largestPrimeFactor :: Int -> Int
largestPrimeFactor n = largestPrimeFactor_ n 2

answer :: Int
answer = largestPrimeFactor 600851475143