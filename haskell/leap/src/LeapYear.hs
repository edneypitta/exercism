module LeapYear (isLeapYear) where

divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = x `rem` y == 0

isLeapYear :: Integer -> Bool
isLeapYear year
  | not $ year `divisibleBy` 4 = False
  | year `divisibleBy` 400 = True
  | year `divisibleBy` 100 = False
  | otherwise = True
                   
