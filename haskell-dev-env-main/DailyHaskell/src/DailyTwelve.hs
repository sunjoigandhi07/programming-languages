module DailyTwelve where 
    -- onlyNothing 
    -- consumes a function and a list 
    -- produces T if the function is applied to the elements in the list 
    onlyNothing :: (a -> Maybe b) -> [a] -> Bool 
    onlyNothing _ [] = True 
    onlyNothing y (x : xs) = case y x of 
        Just _ -> False 
        Nothing -> onlyNothing y xs 

    -- firstAnswer 
    -- consumes a function and a list 
    -- produces Just v or Nothing 
    firstAnswer :: (a -> Maybe b) -> [a] -> Maybe b 
    firstAnswer _ [] = Nothing 
    firstAnswer y (x : xs) = case y x of 
        Just v -> Just v 
        Nothing -> firstAnswer y xs 

    -- allAnswers 
    -- consumes a function and a list 
    -- produces Nothing or Just lstn 
    allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
    allAnswers _ [] = Just []
    allAnswers y (x : xs) = case y x of 
        Just lst -> case allAnswers y xs of 
            Just rest -> Just (lst ++ rest)
            Nothing -> Nothing 
        Nothing -> Nothing 