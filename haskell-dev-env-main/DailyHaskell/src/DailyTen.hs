module DailyTen where
    -- firstFunctorLaw 
    -- consumes a function 
    -- produces a Bool
    firstFunctorLaw :: (Eq (f a), Functor f) => f a -> Bool
    firstFunctorLaw functor = fmap id functor == id functor 

    -- secondFunctorLaw 
    -- consumes a function 
    -- produces a Bool 
    secondFunctorLaw :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool 
    secondFunctorLaw f g functor = fmap (f . g) functor == fmap f (fmap g functor)

