module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = if n <= 0 
              then Nothing 
            else Just $ collatz' n 0

collatz' :: Integer -> Integer -> Integer
collatz' 1 steps = steps
collatz' n steps
  | even n    = collatz' (n `div` 2) incSteps
  | otherwise = collatz' (n * 3 + 1) incSteps
  where incSteps = steps + 1