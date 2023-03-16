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
    -- 


    -- 2
    -- 
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