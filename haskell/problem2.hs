-- Problem 2 - Even Fibonacci numbers
module Problem2 where

fibb :: Int -> Int
fibb 0 = 0
fibb 1 = 1
fibb n = fibb (n - 1) + fibb (n - 2)

sumEvenFibbBellow :: Int -> Int
sumEvenFibbBellow max = sum $ filter even $ takeWhile (< max) [ fibb n | n <- [0..] ]

answer :: Int
answer = sumEvenFibbBellow $ 4 * 10^6
