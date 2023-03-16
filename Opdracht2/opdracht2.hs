module Opdracht2 where
    import Data.Char
    -- 1a 
    -- Searches for the Greatest Common Divider 
    euclid::Integer -> Integer -> Integer
    euclid x y
        | x < y = euclid y x
        | y == 0 = x
        | otherwise = euclid y (x `mod` y)

    -- 1b
    -- Old function
    egcd::Integer -> Integer -> (Integer, Integer, Integer)
    egcd 0 b = (b, 0, 1)
    egcd a b =
        let (g, s, t) = egcd (b `mod` a) a
         in (g, t - (b `div` a) * s, s)
    
    -- New function that adds modulus if outcome is smaller than 0
    egcd':: Integer -> Integer -> (Integer, Integer, Integer)
    egcd' a b
        | s < 0 = (g, s + b, t)
        | t < 0 = (g, s, t + b)
        where (g, s, t) = egcd a b

    -- 2
    -- Generate keys
    generateKeys::Integer -> Integer -> Integer -> (Integer, Integer, Integer)
    generateKeys p1 p2 e
        | not (isPrime p1 || isPrime p2) = (0, 0, 0)
        | not (e < m' || euclid e m' == 1) = (0, 0, 0)
        | otherwise = (e, d, m)
        where m = modulus p1 p2
              m' = modulus' p1 p2
              d = head [x | x <- [1..], (e * x) `mod` m' == 1]
    
    -- Calculate modulus
    modulus::Integer -> Integer -> Integer
    modulus p q = p * q

    -- Calculate modulus'
    modulus'::Integer -> Integer -> Integer
    modulus' p q = (p - 1) * (q - 1)

    -- Check if given number is prime
    isPrime::Integer -> Bool
    isPrime p
        | p > 1 = null [ x | x <- [2..p - 1], p `mod` x == 0]
        | otherwise = False

    -- 3a
    -- Rsa encryption
    rsaencrypt::(Integer, Integer) -> Integer -> Integer
    rsaencrypt (e, m) x = x^e `mod` m

    -- 3b
    -- Rsa decryption
    rsadecrypt::(Integer, Integer) -> Integer -> Integer
    rsadecrypt (d, m) x = x^d `mod` m

    -- 4
    -- Encrypt letter
    encryptLetter::(Integer, Integer) -> Char -> Integer
    encryptLetter (e, m) a = rsaencrypt (e, m) (toInteger $ ord a)

    -- Decrypt letter
    decryptLetter::(Integer, Integer) -> Integer -> Char
    decryptLetter (d, m) a = chr $ fromInteger $ rsadecrypt (d, m) a

    -- 5
    {-
     - Alice versleutelt haar bericht met haar eigen privé sleutel
     - Vervolgens versleutelt ze dit nog eens met de publieke sleutel van Bob
     - Versleutelt bericht wordt verzonden
     - Bob ontsleutelt vervolgens de buitenste laag met zijn eigen privé sleutel
     - Vervolgens ontsleutelt Bob het laatste gedeelte met de publieke sleutel van Alice
     -}