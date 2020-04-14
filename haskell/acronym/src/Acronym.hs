module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate xs = abbreviate' xs 0 ""

abbreviate' :: String -> Int -> String -> String
abbreviate' src currIdx acc 
  | currIdx == 0          = abbreviate' src 1 [head src]
  | currIdx == length src = acc
  | otherwise             = 
    let (prev:curr:_) = prevCurr in
      if isPartOfAcronym prev curr
        then abbreviate' src incCurr $ acc ++ [toUpper curr]
      else abbreviate' src incCurr acc
    where prevCurr = take 2 $ drop (pred currIdx) src
          incCurr  = succ currIdx

isPartOfAcronym :: Char -> Char -> Bool
isPartOfAcronym prev curr
  | not $ isLetter curr = False
  | isUpper prev        = False
  | isUpper curr        = True
  | prev == '\''        = False
  | isPunctuation prev  = True
  | isSpace prev        = True
  | otherwise           = False
