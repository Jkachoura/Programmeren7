import System.Win32 (COORD(yPos), xBUTTON1)
-- 1a 
-- Searches for the Greatest Common Divider
euclid::Integer -> Integer -> Integer
euclid x y
    | x < y = euclid y x
    | y == 0 = x
    | otherwise = euclid y (x `mod` y)