-- Problem 1 - Multiples of 3 and 5

isMultipleOf :: Int -> Int -> Bool
isMultipleOf divisor value = mod value divisor == 0

multOf3Bellow1000 :: [Int]
multOf3Bellow1000 = [ n | n <- [0..999], isMultipleOf 3 n ]

multOf5Bellow1000 :: [Int]
multOf5Bellow1000 = [ n | n <- [0..999], isMultipleOf 5 n ]

multOf15Bellow1000 :: [Int]
multOf15Bellow1000 = [ n | n <- [0..999], isMultipleOf 15 n ]

sumOfAllMultiples :: Int
sumOfAllMultiples = sum multOf3Bellow1000 + sum multOf5Bellow1000 - sum multOf15Bellow1000

-- Problem 2 - Even Fibonacci numbers

fibb :: Int -> Int
fibb 0 = 0
fibb 1 = 1
fibb n = fibb (n - 1) + fibb (n - 2)

-- Sum of all even fibbonacci bellow 4 million
sumFibb :: Int
sumFibb = sum $ filter even $ takeWhile (< 4 * 10^6) [ fibb n | n <- [0..] ]