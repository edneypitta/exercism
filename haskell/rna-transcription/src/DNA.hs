module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = foldr fold $ Right ""

fold :: Char -> Either Char String -> Either Char String
fold _ (Left c)    = Left c
fold 'G' (Right s) = Right $ 'C':s
fold 'C' (Right s) = Right $ 'G':s
fold 'T' (Right s) = Right $ 'A':s
fold 'A' (Right s) = Right $ 'U':s
fold x (Right _)   = Left x