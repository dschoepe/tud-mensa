module TUDMensa.Get (getWeekly) where

import Network.HTTP
import Text.HTML.TagSoup

weeklyUrl :: Request String
weeklyUrl = getRequest "http://www.studentenwerkdarmstadt.de/index.php?option=com_spk&task=stadtmitte&view=week"

getWeekly :: IO [Tag String]
getWeekly = fmap (parseTags . either (const []) rspBody) .  simpleHTTP $ weeklyUrl
