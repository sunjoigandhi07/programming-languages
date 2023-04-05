module DailyOne where
    --function that computes quadratic functions 
    -- takes in four parameters all of type double and returns a double 
    -- uses a square x helper function 
    -- function type: Double -> Double -> Double -> Double -> Double 
    quadratic :: Double -> Double -> Double -> Double -> Double
    square :: Num a => a -> a
    square x = x * x 
    quadratic a b c x =  a + (b * x) + (c * square x)
    
    --function that takes a double and a two tuple of doubles
    -- the tuple of doubles represent a two-dimensional vector 
    -- function returns a two tuple of doubles representing the vector scaled 
    -- by the single double parameter
    -- function type: Double -> (Double, Double) -> (Double, Double)
    scaleVector :: Double -> (Double, Double) -> (Double, Double)
    scaleVector t (x, y) = (t*x, t*y)

    -- function that takes two three tuples representing three dimentional points 
    -- finds and returns the cartesian distance using the formula d = sqrt((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)
    -- function type: (Double, Double, Double) -> (Double, Double, Double) -> Double
    tripleDistance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
    tripleDistance (0, 0, 0) (0, 0, 0) = 0
    tripleDistance (x1, y1, z1) (x2, y2, z2) = sqrt((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)