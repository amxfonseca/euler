-- Problem 9 - Special Pythagorean triplet
module Problem9 where

import Problem7 (isqrt)


-- We use a fairly naive implementation of Euclid's formula to find a Pythagorean triples whose sum is equal to n
findTripleWithSum :: Int -> [Int]
findTripleWithSum n =
    head $ dropWhile ((/= n) . sum)
    [
        [m^2 - n^2, 2 * m * n, m^2 + n^ 2] |
        n <- [2..limit],
        m <- [succ n..limit], even n /= even m, m `mod` n == 0
    ]
    where
        limit = isqrt $ div n 2

answer :: Int
answer =
    product $ findTripleWithSum 1000
