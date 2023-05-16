module DailyEight where 
    data Event = Event {
        name :: String, 
        day :: Int, 
        month :: String, 
        year :: Int, 
        xlocation :: Double, 
        ylocation :: Double
    } deriving ( Show )

    -- inYear 
    -- consumes a year and a list of events 
    -- produces the event structure of the events that occured in the given year 
    inYear :: Int -> [Event] -> [Event]
    inYear _ [] = [] 
    inYear a events = filter (\event -> year event == a) events 
 
    -- inDayRange 
    -- consumes a start day, end day, and list of events 
    -- produces a new list of events that occurred between the start and end day 
    inDayRange :: Int -> Int -> [Event] -> [String]
    inDayRange start end events =  foldr (\event names -> if (day event >= start) && (day event <= end) then name event : names else names) [] events

    -- inArea 
    -- consumes a name, a lower x location, an upper x location, a lower y location, an upper y location 
    -- produces a new list of events that occured in that specified area
    inArea :: String -> Double -> Double -> Double -> Double -> [Event] -> [Event]
    inArea name xlower xupper ylower yupper = filter (\e -> eventName e == name && 
        xLocation e >= xlower && xLocation e <= xupper && yLocation e >= ylower && yLocation e <= yupper)
        where eventName (Event n _ _ _ _ _) = n
              xLocation (Event _ _ _ _ x _) = x
              yLocation (Event _ _ _ _ _ y) = y


