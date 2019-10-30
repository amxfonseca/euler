-- Problem 10 - Summation of primes
module Problem10 where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
  sieve <- newArray (2, n) True -- All elements are prime

  forM_ [2..n] $ \i -> do
    isPrime <- readArray sieve i
    when isPrime $
      forM_ [i*2, i*3..n] $ -- Set all multiples of i to not prime
        \j -> writeArray sieve j False

  return sieve

primesUnder :: Int -> [Int]
primesUnder n =
  fmap fst $ filter snd $ assocs $ sieve n


answer :: Int
answer =
  sum $ primesUnder (2 * 10^6)
