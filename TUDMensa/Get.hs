module TUDMensa.Get (getWeekly) where

import Control.Applicative
import Data.Char
import TUDMensa.Types
import Network.HTTP
import Text.HTML.TagSoup

baseUrl :: String
baseUrl = "http://www.studentenwerkdarmstadt.de/index.php?option=com_spk&task="

weeklyUrl :: Date -> Location -> Request String
weeklyUrl NextWeek loc = getRequest $ baseUrl ++ tell loc ++ "&view=nextweek"
weeklyUrl _ loc = getRequest $ baseUrl ++ tell loc ++ "&view=week"

tell = map toLower . show

getWeekly :: Date -> Location -> IO [Tag String]
getWeekly =
  (.) (fmap (parseTags . either (const []) rspBody) . simpleHTTP) . weeklyUrl
