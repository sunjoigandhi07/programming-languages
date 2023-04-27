module DailySix where
    import Data.Maybe 
    -- shorterThan 
    -- consumes a number and a list of strings
    -- produces a list of words whose length is shorter than or equal to the number 
    shorterThan :: Int -> [String] -> [String]
    shorterThan n = filter(\ word -> length word <= n)


    -- removeMultiples 
    -- consumes a number and a list of numbers 
    -- produces a list of numbers with no muliples of the given number 
    removeMultiples :: Int -> [Int] -> [Int]
    removeMultiples p = filter (\q -> q `mod` p /= 0)

    -- onlyJust 
    -- consumes a list of Maybe a 
    -- produces a list of values where the value of Nothing has been removed 
    onlyJust :: [Maybe a] -> [Maybe a]
    onlyJust es = filter isJust es 
        where isJust Nothing = False 
              isJust _ = True  

