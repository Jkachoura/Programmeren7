
module Opdracht3 where
    import Data.List(group,nub)
    --1a
    differentieer :: (Double -> Double) -> Double -> Double -> Double
    differentieer f p x = (f (x + p) - f (x)) / p

    -- 1b
    -- Riemann left sum algorithm
    integreer::(Double -> Double) -> Double -> Double -> Double -> Double
    integreer f a b p =
        let dx = (b - a) / (1/ p)
        in dx * (sum [f x | x <- [a,a+dx..b-dx]])



    --2
    dubbelen' :: (Eq a) => [a] -> [a]
    dubbelen' s = nub $ concat $ filter ((>1)  . length) $ group  s


