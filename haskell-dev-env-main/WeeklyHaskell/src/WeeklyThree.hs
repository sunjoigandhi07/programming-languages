module WeeklyThree where 
    -- new data type that contains Double values 
    data Vec = Vec [Double] deriving ( Show )
    
    -- functions for dealing with numbers 
    instance Num Vec where 
        -- adding 
        (+) :: Vec -> Vec -> Vec
        (Vec xs) + (Vec ys) = Vec (zipWith (+) xs ys)
        -- subtracting 
        (-) :: Vec -> Vec -> Vec
        (Vec xs) - (Vec ys) = Vec (zipWith (-) xs ys)
        -- multiplication
        (*) :: Vec -> Vec -> Vec
        (Vec xs) * (Vec ys) = Vec (zipWith (*) xs ys)
        abs :: Vec -> Vec
        abs (Vec xs) = Vec (map abs xs)
        signum :: Vec -> Vec
        signum (Vec xs) = Vec (map signum xs)
        fromInteger :: Integer -> Vec
        fromInteger n = Vec (repeat (fromInteger n))

    -- instantiates Vec as a member of Eq 
    instance Eq Vec where 
        (==) :: Vec -> Vec -> Bool
        (Vec xs) == (Vec ys) = and (zipWith (==) xs ys)

    -- instantiate Ord typeclass and 
    -- defines compare function 
    instance Ord Vec where 
        compare :: Vec -> Vec -> Ordering
        compare (Vec xs) (Vec ys) = compare (sum xs) (sum ys)
    
    -- creates a new typeClass called VecT 
    -- contains one function, magnitude 
    class VecT a where 
        magnitude :: a -> Double 
    
    --instantiates Vec as a VecT
    -- defines the magnitude for the vector 
    instance VecT Vec where 
        magnitude :: Vec -> Double
        magnitude (Vec xs) = sqrt (sum (map (^2) xs))
    
    -- instantiates Vec as a semigroup 
    -- uses addition as the associative operator 
    instance Semigroup Vec where 
        (<>) :: Vec -> Vec -> Vec
        (Vec xs) <> (Vec ys) = Vec (zipWith (+) xs ys)

    instance Monoid Vec where 
        mempty = Vec (repeat 0.0)