module WeeklyHaskellOne where
    -- function takes in a single char and a string 
    -- function removes the char from the string and returns the new string 
    -- function type: Char -> String -> String 
    removeChar :: Char -> String -> String
    removeChar a = foldr (\x acc -> if x == a then acc else x:acc) []
    -- function takes in a string and returns a new string 
    -- function removes spaces, tabs, and new line characters from the input string 
    -- function type: String -> String 
    removeWhitespace :: String -> String 
    removeWhitespace = removeChar ' ' . removeChar '\t' . removeChar '\n' . removeChar '\r' 

    -- function takes in a string and returns a new string 
    -- function removes chars such as , . () [] {}
    -- function type: String -> String 
    removePunctuation :: String -> String 
    removePunctuation = removeChar ',' . removeChar '.' . removeChar '(' . removeChar ')' . removeChar '[' . removeChar ']' . removeChar '{' . removeChar '}' 
    
    -- function takes in a string and returns an int
    -- function takes the string and returns each character's ascii values 
    -- function type: String -> [Int] 
    charsToAscii :: String -> [Int]
    charsToAscii = map fromEnum

    -- function takes in a list of ints and returns a string 
    -- function takes in ascii values and returns the list of characters the ints represnt 
    -- function type: [Int] -> [Char ]
    asciiToChars :: [Int] -> [Char]
    asciiToChars  = map toEnum

    -- function takes in an int and a list of ints 
    -- function returns a new list of ints 
    -- function shifts valid ascii values 0-127 by the shift value 
    -- function type: Int -> [Int] -> [Int] 
    shiftInts :: Int -> [Int] -> [Int]
    shiftInts shift = map (\x -> (x + shift) `mod` 128)

    -- function takes in an int and a string 
    -- function returns a new string 
    -- function encodes the original string and produces an encrypted message as the return string 
    -- function type: Int -> String -> String 
    shiftMessage :: Int -> String -> String 
    shiftMessage shift = map (\x -> toEnum $  (fromEnum x + shift) `mod` 128)


