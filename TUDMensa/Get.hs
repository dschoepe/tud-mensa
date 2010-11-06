module TUDMensa.Get (getWeekly) where

import TUDMensa.Types
import Network.HTTP
import Text.HTML.TagSoup

baseUrl :: String
baseUrl = "http://www.studentenwerkdarmstadt.de/index.php?option=com_spk&task=stadtmitte&view="

weeklyUrl :: Date -> Request String
weeklyUrl NextWeek = getRequest $ baseUrl ++ "nextweek"
weeklyUrl _ = getRequest $ baseUrl ++ "week"

getWeekly :: Date -> IO [Tag String]
getWeekly = fmap (parseTags . either (const []) rspBody) . simpleHTTP . weeklyUrl
