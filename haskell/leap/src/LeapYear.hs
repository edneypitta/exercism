module LeapYear (isLeapYear) where

divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = x `rem` y == 0

isLeapYear :: Integer -> Bool
isLeapYear year
  | year `divisibleBy` 100 && year `divisibleBy` 400 = True
  | year `divisibleBy` 100 = False
  | year `divisibleBy` 4 = True
  | otherwise = False
                   
