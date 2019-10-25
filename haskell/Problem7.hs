module Problem7 where

isqrt :: Integral a => a -> Int
isqrt =
  floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime k
  | k < 2 = False
  | otherwise = null [ n | n <- [2..isqrt k], k `mod` n == 0]

nthPrime :: Int -> Int
nthPrime =
  (!!) [ p | p <- [2..], isPrime p ] . pred

answer :: Int
answer = nthPrime 10001