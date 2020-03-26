{-# LANGUAGE InstanceSigs #-}

module Block1.Task1
  ( WeekDay (..)
  , afterDays
  , daysToParty
  , nextDay
  , isWeekend
  ) where

data WeekDay = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday deriving Show

instance Enum WeekDay where
  toEnum :: Int -> WeekDay
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
  toEnum _ = error "Outside of boundaries"

  fromEnum :: WeekDay -> Int
  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

instance Eq WeekDay where
  x == y = fromEnum x == fromEnum y

-- | Get the next day after the given WeekDay.
nextDay :: WeekDay -> WeekDay
nextDay = afterDays 1

-- | Get the day that is given number of days after the given WeekDay.
afterDays :: Int -> WeekDay -> WeekDay
afterDays count day = toEnum $ (fromEnum day + count) `mod` 7

-- | Check if the given WeekDay is a weekend.
isWeekend :: WeekDay -> Bool
isWeekend day = day == Saturday || day == Sunday

-- | Return the amount of days from the given WeekDay to Friday.
daysToParty :: WeekDay -> Int
daysToParty day =
  if difference < 0
  then 7 + difference
  else difference
    where difference = fromEnum Friday - fromEnum day
