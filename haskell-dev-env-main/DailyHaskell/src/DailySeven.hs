module DailySeven where
    -- findLongest 
    -- consumes a list of strings 
    -- produces the first word whose length is greater than or equal to the length of the words in the list 
    findLongest :: [String] -> String
    findLongest fs = foldr (\f str -> if length f >= length str then f else str) ""  fs

    -- anyLarger 
    -- consumes a number and a list of numbers 
    -- produces T if list of numbers contains a number greater than or equal to the input number 
    anyLarger :: Integer -> [Integer] -> Bool
    anyLarger v es = foldl (\val e -> val || e >= v) False es

    -- allNames 
    -- consumes a list of tuples containing a first and last name 
    -- produces a string of the first and last names in the list, full names separated by commas 
    allNames :: [(String, String)] -> String
    allNames ns = foldr (\(first, last) full -> first ++ " " ++ last ++ if null full then "" else ", " ++ full) "" ns
