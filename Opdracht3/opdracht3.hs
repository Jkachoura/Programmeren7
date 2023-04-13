module Opdracht3 where
    -- import Data.List(group,nub,(\\))
    import Data.List
    --1a
    -- Differentiequotient
    differentieer :: (Double -> Double) -> Double -> Double -> Double
    differentieer f p x = (f (x + p) - f (x)) / p

    -- 1b
    -- Riemann left sum algorithm
    integreer::(Double -> Double) -> Double -> Double -> Double -> Double
    integreer f a b p =
        let dx = (b - a) / (1 / p)
        in dx * (sum [f x | x <- [a,a+dx..b-dx]])

    -- 2
    -- List met element die meer dan 1 keer voorkomen
    dubbelen::Eq a => [a] -> [a]
    dubbelen s = nub (s \\ nub s)

    -- 3
    mogelijkheden = [[a, b, c, d, e] | a <- [1..6], b <- [1..6], c <- [1..6], d <- [1..6], e <- [1..6]]

    hoeveelheidOgen :: Ord b => [b] -> [(Int, b)]
    hoeveelheidOgen x = zip (map length (group (sort x))) (nub ((sort x))) -- results in [(no. of indices, eyes)]

    filterOgen :: Ord b => [b] -> Int -> [(Int, b)]
    filterOgen x y = filter (\(p,_) -> p==y) (hoeveelheidOgen x) -- results in [(y, eyes)]

    filterStraight x = if sort x == [1..5] || sort x == [2..6] then True else False

    poker list = filter (\x -> length (filterOgen x 5) == 1) list
    fourOfAKind list = filter (\x -> length (filterOgen x 4) == 1) list
    threeOfAKind list = filter (\x -> length (filterOgen x 3) == 1 && length (filterOgen x 2) == 0) list
    fullHouse list = filter (\x -> length (filterOgen x 2) == 1 && length (filterOgen x 3) == 1) list
    twoPair list = filter (\x -> length (filterOgen x 2) == 2) list
    onePair list = filter (\x -> length (filterOgen x 2) == 1 && length (filterOgen x 3) == 0) list
    straight list = filter filterStraight list
    bust list = filter (\x -> length ((poker [x]) ++ (fourOfAKind [x]) ++ (threeOfAKind [x]) ++ (fullHouse [x]) ++ (twoPair [x]) ++ (onePair [x]) ++ (straight [x])) == 0) list
    
    pokerKanzen = [(fromIntegral (length $ poker (mogelijkheden)) / fromIntegral (length mogelijkheden)) * 100,
                   (fromIntegral (length $ fourOfAKind (mogelijkheden)) / fromIntegral (length mogelijkheden)) * 100,
                   (fromIntegral (length $ threeOfAKind (mogelijkheden)) / fromIntegral (length mogelijkheden)) * 100,
                   (fromIntegral (length $ twoPair (mogelijkheden)) / fromIntegral (length mogelijkheden)) * 100,
                   (fromIntegral (length $ onePair (mogelijkheden)) / fromIntegral (length mogelijkheden)) * 100,
                   (fromIntegral (length $ straight (mogelijkheden)) / fromIntegral (length mogelijkheden)) * 100,
                   (fromIntegral (length $ bust (mogelijkheden)) / fromIntegral (length mogelijkheden)) * 100]