module Bob (responseFor) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char

responseFor :: Text -> String
responseFor text
  | nothing             = "Fine. Be that way!"
  | question && yelling = "Calm down, I know what I'm doing!"
  | question            = "Sure."
  | yelling             = "Whoa, chill out!"
  | otherwise           = "Whatever."
  where nothing  = T.null . T.strip $ text
        question = T.last (T.strip text) == '?'
        yelling  = let letters = T.filter isLetter text 
                   in not (T.null letters) && T.all isUpper letters