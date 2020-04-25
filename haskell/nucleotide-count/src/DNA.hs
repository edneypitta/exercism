module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    case mns of Nothing -> Left "invalid nucleotides"
                Just ns -> Right $ M.fromListWith (+) $ empty ++ counts ns
    where mns       = traverse toNucleotide xs
          empty     = [(A, 0), (C, 0), (G, 0), (T, 0)]
          counts ns = zip ns $ repeat 1

toNucleotide :: Char -> Maybe Nucleotide
toNucleotide 'A' = Just A
toNucleotide 'C' = Just C
toNucleotide 'G' = Just G
toNucleotide 'T' = Just T
toNucleotide _   = Nothing
