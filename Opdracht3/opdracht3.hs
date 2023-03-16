module Opdracht3 where
    
    --1a
    differentieer :: (Double -> Double) -> Double -> Double -> Double
    differentieer f p x = (f (x + p) - f (x - p)) / (2 * p)


