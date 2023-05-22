module DailyEightSpec where

import DailyEight
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  --describe "inYear" $ do
    --let events =
          --[ Event "Event 1" 1 "Jul" 2011 10 10,
           -- Event "Event 2" 4 "Aug" 2012 20 20,
           -- Event "Event 3" 17 "Jan" 2011 40 40,
           -- Event "Event 4" 28 "Feb" 2011 30 30,
           -- Event "Event 5" 30 "Mar" 2023 50 50
          --]
    --it "produces a list of events during a certain year" $
       -- inYear 2012 events `shouldBe` [Event {name = "Event 2", day = 4, month = "Aug", year = 2012, xlocation = 20.0, ylocation = 20.0}] 
    --it "produces a list of events during a certain year" $
        --inYear 2023 events `shouldBe` [Event {name = "Event 5", day = 30, month = "Mar", year = 2023, xlocation = 50.0, ylocation = 50.0}]
   -- it "produces a list of events during a certain year" $
        --inYear 2011 events `shouldBe` [Event {name = "Event 1", day = 1, month = "Jul", year = 2011, xlocation = 10.0, ylocation = 10.0}, Event {name = "Event 3", day = 17, month = "Jan", year = 2011, xlocation = 40.0, ylocation = 40.0}, Event {name = "Event 4", day = 28, month = "Feb", year = 2011, xlocation = 30.0, ylocation = 30.0}]
  describe "inDayRange" $ do
    let events =
          [ Event "Event 1" 1 "Apr" 2012 10 10,
            Event "Event 2" 7 "Apr" 2012 20 20,
            Event "Event 3" 29 "Apr" 2012 30 30,
            Event "Event 4" 14 "Apr" 2012 40 40
          ]
    it "produces a list of events within the day range" $
        inDayRange 1 10 events `shouldBe` ["Event 1", "Event 2"]
    it "produces a list of events within the day range" $
        inDayRange 9 13 events `shouldBe` []
    it "produces a list of events within the day range" $
        inDayRange 14 30 events `shouldBe` ["Event 3", "Event 4"]
  --describe "inArea" $ do
    --let events =
          --[ Event "kiki" 1 "Apr" 2012 10 10,
            --Event "kiki" 7 "Apr" 2012 20 20,
            --Event "kiki" 29 "Apr" 2012 30 30,
            --Event "kiki" 14 "Apr" 2012 40 40
         -- ]
    --it "produces a list of events in a specified area" $
        --inArea "kiki" 10 30 10 30 events `shouldBe` [Event {name = "kiki", day = 1, month = "Apr", year = 2012, xlocation = 10.0, ylocation = 10.0}, Event {name = "kiki", day = 7, month = "Apr", year = 2012, xlocation = 20.0, ylocation = 20.0}, Event {name = "kiki", day = 29, month = "Apr", year = 2012, xlocation = 30.0, ylocation = 30.0}]
    --it "produces a list of events in a specified area" $
        --inArea "kiki" 10 10 30 30 events `shouldBe` []
    --it "produces a list of events in a specified area" $
        --inArea "kiki" 20 40 20 40 events `shouldBe` [Event {name = "kiki", day = 7, month = "Apr", year = 2012, xlocation = 20.0, ylocation = 20.0}, Event {name = "kiki", day = 29, month = "Apr", year = 2012, xlocation = 30.0, ylocation = 30.0}, Event {name = "kiki", day = 14, month = "Apr", year = 2012, xlocation = 40.0, ylocation = 40.0}]