module DailyTwo where 
    -- function parameters take in a list 
    --function returns a list of every 4th element in the list 
    -- function type [a] -> [a]
    every4th :: [a] -> [a]
    every4th [] = []
    every4th [elem] = []
    every4th [_, elem] = []
    every4th [_, _, elem] = []
    every4th (_:_:_: elem:elems) = elem : every4th elems
    
    -- function parameters take 2 lists of numbers 
    -- returns the dot quuotient of the 2 lists 
    -- function type: [Float] -> [Float] -> Float
    tupleDotQuotient :: [Double] -> [Double] -> Double
    -- base case 
    tupleDotQuotient [] [] = 0.0
    tupleDotQuotient (x:xs) (y:ys) = (x / y) + (tupleDotQuotient xs ys)

    -- function takes in a string and a list of strings 
    -- returns the list of strings with the parameter string at the end of every string in the list 
    -- function type: String -> [String] -> [String]
    appendToEach :: String -> [String] -> [String]
    appendToEach _ [] = [] 
    appendToEach s (elem : elems) = (elem ++ s) : (appendToEach s elems)

    -- function parameters takes in a list of numbers 
    -- returns a list of numbers 
    -- searches for duplicates within the list and returns a list of all the numbers only once 
    -- function type: [Integer] -> [Integer]
    toSetList :: [Integer] -> [Integer]
    toSetList [] = []
    toSetList (element: elements) 
        -- skip x if there is a duplicate 
        | element `elem` elements = toSetList elements 
        -- add x to the return list by using cons : and continue through the rest 
        | otherwise = element : toSetList elements