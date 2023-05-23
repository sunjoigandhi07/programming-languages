module DailyEleven where 

    -- allLefts 
    -- consumes a list of Either types 
    -- produces a list of any Left values  
    allLefts :: [Either a b] -> [a]
    allLefts [] = []
    allLefts (Left x : xs) = x : allLefts xs 
    allLefts (_ : xs) = allLefts xs

    -- produceStringOrSum 
    -- consumes two Either types
    -- produces the sum of the two parameters if they are both integers 
    produceStringOrSum :: (Either String Integer) -> (Either String Integer) -> (Either String Integer)
    produceStringOrSum (Left x) _ = Left x 
    produceStringOrSum _ (Left y) = Left y 
    produceStringOrSum (Right x) (Right y) = Right (x + y)

    -- sumListOfEither 
    -- consumes a list consisting of Either Strings of Integers
    -- produces the first String in the list or the sum of all the intergers in the list 
    sumListOfEither :: [Either String Integer] -> (Either String Integer)
    sumListOfEither = foldl sum (Right 0)
        where 
            sum (Left x) _ = Left x 
            sum _ (Left y) = Left y 
            sum (Right acc) (Right x) = Right (acc + x)
        