module Opdracht1 where
    import Data.Bits
    -- 1a
    -- Calculate faculty with pattern matching
    faca::Int -> Int
    faca 0 = 1
    faca x = product [1..x]

    -- 1b
    -- Calculate faculty with guards
    facb:: Int -> Int
    facb x
        | x == 0 = 1
        | otherwise = product [1..x]
    
    -- 2a
    -- Calculate root of quadratic function
    nulpuntena:: Double -> Double -> Double -> [Double]
    nulpuntena a b c = 
        let d = b^2 - (4 * a * c)
            x1 = (-b + sqrt(d)) / (2 * a)
            x2 = (-b - sqrt(d)) / (2 * a)
        in if d < 0
                then []
            else if d == 0
                then[x1]
            else
                [x1, x2]

    -- 2b
    -- Calculate root of quadratic function with where and guards
    nulpuntenb:: Double -> Double -> Double -> [Double]
    nulpuntenb a b c
        | d < 0 = []
        | d == 0 = [x1]
        | otherwise = [x1, x2]
        where d = b^2 - (4 * a * c)
              x1 = (-b + sqrt(d)) / (2 * a)
              x2 = (-b - sqrt(d)) / (2 * a)

    -- 2c
    -- Give amount of possible throws with 3 dices where the outcome is a multiple of 5
    veelvoud5:: Int
    veelvoud5 = length[(a, b, c) | a <- [1..6], b <- [1..6], c <- [1..6], (a + b + c) `mod` 5 == 0]

    -- 2d
    -- Give amount of possible throws with 3 dices where the outcome is a multiple of n
    veelvoudn :: Integral c => c -> Int
    veelvoudn n = length[(a, b, c) | a <- [1..6], b <- [1..6], c <- [1..6], (a + b + c) `mod` n == 0]

    -- 3
    -- Give 3 numbers a, b, c where a == (2 * abs(b - c)), b == a * c, c == ((a + b) / 2)
    -- Answer a, b and c == 0
    puzzel = [(a, b, c) | a <- [0..100], b <- [0..100], c <- [0..100], a == (2 * abs(b - c)), b == a * c, c == ((a + b) / 2)]

    -- 4a
    -- Calculate product with recurrent addition
    -- Overflow error:
    mult:: Integer -> Integer -> Integer
    mult x y = if x == 0 || y == 0 then 0 else x + mult x (y-1)

    -- 4b
    -- Calculate product with bitshifting
    fastmult:: Integer -> Integer -> Integer
    fastmult x y
        | x == 1 = y
        | x .&. 1 == 0 = fastmult (shiftR x 1) (shiftL y 1)
        | otherwise = y + fastmult (shiftR x 1) (shiftL y 1)

    --5a 
    -- Calculate power using recursion
    pow:: Integer -> Integer -> Integer
    pow x n
        | n == 0  =  1
        | otherwise = x * pow x (n-1)

    --5b
    -- Calculate power using bitshifting
    fastpow:: Integer -> Integer -> Integer
    fastpow x p
        | p == 0 = 1
        | p == 1 = x
        | p .&. 1 == 0 = fastpow (x * x) (shiftR p 1)
        | otherwise = x * fastpow x (p - 1)