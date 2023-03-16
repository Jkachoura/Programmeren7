module Opdracht3 where
    import Data.List (group)
    
    --1a
    differentieer :: (Double -> Double) -> Double -> Double -> Double
    differentieer f p x = (f (x + p) - f (x - p)) / (2 * p)



    --1b


    --2
    dubbelen :: Eq a => [a] -> [a]
    dubbelen xs = concat $ filter (\g -> length g > 1) (group xs)
