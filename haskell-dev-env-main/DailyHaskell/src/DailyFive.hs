module DailyFive where
    import Data.Char (isLower)

    -- multPairs 
    -- consumes a list of pairs of integers 
    -- produces a list of the products of each pair 
    multPairs :: [(Int, Int)] -> [Int]
    multPairs [] = []
    multPairs xs = map (\(a,b) -> a * b) xs

    -- squareList 
    -- consumes a list of integers 
    -- produces a new list with a new pair, first the original int, second the square of that int 
    squareList :: [Int] -> [(Int, Int)]
    squareList [] = []
    squareList elems = map (\elem -> (elem, elem * elem)) elems

    -- findLowercase 
    -- consumes a list of Strings 
    -- produces a list of Bools, T if first letter of string is lowercase, F if first letter of string is uppercase 
    findLowercase :: [String] -> [Bool]
    findLowercase [] = [] 
    findLowercase ys = map (\y -> isLower (head y)) ys
