module DailyThree where
    -- function takes in an element and a list of elements 
    -- function returns everything in the list equal to the parament element 
    -- function type: Eq a => a -> [a] -> [a]
    removeAllExcept :: Eq a => a-> [a] -> [a]
    removeAllExcept _ [] = []
    removeAllExcept x (elem:elems)
        | x == elem     = x : removeAllExcept x elems
        | otherwise = removeAllExcept x elems

    -- function takes in an element and a list of elements 
    -- function returns an integer representing how often the element occurs in the list 
    -- function type: Eq a => a -> [a] -> Int 
    countOccurances :: Eq a => a -> [a] -> Int 
    countOccurances _ [] = 0
    countOccurances x (y:ys) 
        | x == y    = 1 + countOccurances x ys 
        | otherwise = countOccurances x ys

    -- function takes in two elements and a list of elements 
    -- function returns a new list with the second element parameter replacing the first parameter within the list 
    -- function type: Eq a => a -> a -> [a] -> [a]
    substitute :: Eq a => a -> a -> [a] -> [a]
    substitute _ _ [] = []
    substitute old new (elem:elems)
        | old == elem = new : substitute old new elems 
        | otherwise = elem : substitute old new elems