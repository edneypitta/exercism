module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = let loweredText = map toLower text
                 in all (`elem` loweredText) alphabet 

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"