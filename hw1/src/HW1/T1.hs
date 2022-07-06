module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import GHC.Natural (Natural)

data Day =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Sunday
  | Saturday
  deriving Show

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay Sunday    = Monday   
nextDay Monday    = Tuesday    
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday       
nextDay Thursday  = Friday      
nextDay Friday    = Saturday 
nextDay Saturday  = Sunday   

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays n = (!! fromEnum n) . iterate nextDay

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty d      = 1 + daysToParty (nextDay d)