module DailyFour where
    -- zip3 Lists
    --  consumes 3 lists 
    --  returns a list of 3, tuples 
    zip3Lists :: [a] -> [b] -> [c] -> [(a, b, c)]
    zip3Lists [][][]= []
    zip3Lists (e:es) (f:fs) (g:gs) = (e, f, g) : zip3Lists es fs gs

    -- unzipTriples 
    -- consumes a list of 3, tuples 
    -- returns a tuple of three lists 
    unzipTriples :: [(a, a, a)] -> ([a], [a], [a])
    unzipTriples [] = ([], [], [])
    unzipTriples ((x, y, z):xs) =
        let (xs', ys', zs') = unzipTriples xs
            in  
                (x:xs', y:ys', z:zs')

    -- mergeSorted 3
    -- consumes 3 ordered lists 
    -- returns one ordered list of the 3 ordered lists combined 
    mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
    mergeSorted3 [] [] [] = []
    mergeSorted3 [] [] zs = zs
    mergeSorted3 [] ys [] = ys
    mergeSorted3 xs [] [] = xs
    mergeSorted3 [] (y:ys) (z:zs)
        | y <= z    = y : mergeSorted3 [] ys (z:zs)
        | otherwise = z : mergeSorted3 [] (y:ys) zs
    mergeSorted3 (x:xs) [] (z:zs)
        | x <= z    = x : mergeSorted3 xs [] (z:zs)
        | otherwise = z : mergeSorted3 (x:xs) [] zs
    mergeSorted3 (x:xs) (y:ys) []
        | x <= y    = x : mergeSorted3 xs (y:ys) []
        | otherwise = y : mergeSorted3 (x:xs) ys []
    mergeSorted3 (x:xs) (y:ys) (z:zs)
        | x <= y && x <= z = x : mergeSorted3 xs (y:ys) (z:zs)
        | y <= x && y <= z = y : mergeSorted3 (x:xs) ys (z:zs)
        | otherwise        = z : mergeSorted3 (x:xs) (y:ys) zs


   