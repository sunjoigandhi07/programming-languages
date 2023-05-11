module DailyNine where 

    -- findSmallest 
    -- consumes an ordered list 
    -- produces Nothing if the list is empty or Just v, v being the smallest in the list 
    findSmallest :: (Ord a) => [a] -> Maybe a 
    findSmallest [] = Nothing 
    findSmallest (f:fs) = Just (foldr min f fs)
    
    -- allTrue 
    -- consumes a list of Bools 
    -- produces a Nothing if the list is empty, Just True is all elements are True, Just False otherwise 
    allTrue :: [Bool] -> Maybe Bool 
    allTrue [] = Nothing 
    allTrue fs = if all (==True) fs 
                 then  Just True 
                 else  Just False 

    -- countAllVotes 
    -- consumes a list of Maybe Bool 
    -- produces a tuple of triples showing the # of representatives who have not yet voted, voted in favor, and voted against 
    countAllVotes :: [Maybe Bool] -> (Integer, Integer, Integer) 
    countAllVotes fs = foldl count (0, 0, 0) fs 
        where 
            count (n, y, a) Nothing = (n+1, y, a)
            count (n, y, a) (Just True) = (n, y+1, a)
            count (n, y, a) (Just False) = (n, y, a+1)
