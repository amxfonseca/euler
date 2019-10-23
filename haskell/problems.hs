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

--Problem 3 - Largest prime factor

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

-- Problem 4 - Largest palindrome product

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
