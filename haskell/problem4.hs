-- Problem 4 - Largest palindrome product
module Problem4 where

reverseNum_ :: Int -> Int -> Int
reverseNum_ 0 sum = sum
reverseNum_ n sum =
  reverseNum_ quotient (sum * 10 + remainder)
  where
    (quotient, remainder) = divMod n 10

reverseNum :: Int -> Int
reverseNum = flip reverseNum_ 0

isPalindrome :: Int -> Bool
isPalindrome n = n == reverseNum n

largestPalidromeFromProduct :: Int -> Int
largestPalidromeFromProduct max =
  maximum $
    filter isPalindrome $
    uncurry (*) <$> [ (k,l) | k <- range, l <- range, l >= k ]
  where
    range = [max, pred max..1]

answer :: Int
answer = largestPalidromeFromProduct 999