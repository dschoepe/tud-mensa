{-# LANGUAGE NoMonomorphismRestriction #-}
module Mensa.Parse where

import Control.Arrow
import Control.Exception
import Control.Monad
import Data.List
import Data.Generics
import qualified Data.Map as M
import Text.Regex
import Text.HTML.TagSoup
import Text.StringLike
import Mensa.Types
import Safe

import Debug.Trace

rowCount :: Int
rowCount = 9

findTable :: [Tag String] -> [Tag String]
findTable = takeWhile (not . (~== "</table>")) .
            headDef (throw (ErrorCall "no table found")) .
            partitions (~== "<table class=spk_table>")

dissectTable :: [Tag String] -> [[Tag String]]
dissectTable = take rowCount . drop 1 . partitions (~== "<tr>")

dissectRow :: [Tag String] -> (String, [Meal])
dissectRow = toMeals . splitAt 1 . partitions (~== "<td>")
  where toMeals (titleTags, mealTags) =
          (innerText (head titleTags), map parseMeal mealTags)

parseMeal :: [Tag String] -> Meal
parseMeal tags = Meal (innerText tags)
                      (maybe Unknown parseType .
                       maybeAttrib "alt" . (!!1) $ tags)

mealsPerDay :: [Tag String] -> [(Day, [Meal])]
mealsPerDay = zip [Monday .. Friday] . transpose . map (snd . dissectRow) .
              dissectTable . findTable

parseWeek :: [Tag String] -> WeekMenu
parseWeek = M.fromList .
            everywhere (mkT $ map prettify . filter notEmpty) .
            map (second toDayMenu) . mealsPerDay
  where toDayMenu (bistro:soup:gabel:wok:ottob) = DayMenu { bistro = [bistro]
                                                          , soup = [soup]
                                                          , gabel = [gabel]
                                                          , wok = [wok]
                                                          , ottob = ottob }
        toDayMenu xs = DayMenu [] [] [] [] xs -- this shouldn't happen
        notEmpty = not . ("\160\n" `isPrefixOf`) . dish

prettify :: Meal -> Meal
prettify m = m { dish = removeSoupIntro . removeNewline . removePrice $ dish m }
  where removePrice str = regsub " [A-Z]+ [0-9],[0-9]{2}.*" str ""
        removeNewline str = regsub " *\128 ?\n+" str ""
        removeSoupIntro str = regsub "Von unserer Suppenbar! *" str ""
        regsub expr = subRegex (mkRegex expr)

parseType :: String -> MealType
parseType "rindschwein" = BeefAndPork
parseType "rind" = Beef
parseType "schwein" = Pork
parseType "fleischlos" = Vegetarian
parseType "fisch" = Fish
parseType "lamm" = Lamb
parseType "gefluegel" = Poultry
parseType x = Unknown

maybeAttrib :: (StringLike str, Show str, Eq str) => str -> Tag str -> Maybe str
maybeAttrib att (TagOpen _ atts) = lookup att atts
maybeAttrib _ _ = Nothing

test f = fmap (f . parseTags) $ readFile "test.html"
