{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import Mensa.Parse
import Mensa.Get
import Mensa.Types

import System.Console.CmdArgs
import Data.Generics
import Data.Time
import Data.Time.Calendar.WeekDate
import Text.Printf
import qualified Data.Map as M

data Date = Week | Today deriving (Data, Typeable, Show, Eq)
data Options = Options { date :: Date
                       , hide :: [MealType] } deriving (Data, Typeable, Show)

opts = Options { date = enum [Today &= help "Show data for today"
                             ,Week &= help "Show data for entire week"]
               , hide = def &= typ "[Type of Meal]"
                            &= help hideHelp }
  where hideHelp = "Don't show these types of food in the result. "++
                   "If specified multiple times all specified types will be hidden"

instance Default MealType where
  def = Vegetarian

getMenu = parseWeek `fmap` getWeekly

filterByOpts (Options{..}) = everywhere (mkT $ filter byType)
  where byType = (`notElem` hide) . mealType

getMenu' opts = filterByOpts opts `fmap` getMenu

handleCommand :: Options -> IO ()
handleCommand opts | date opts == Today = do
  (_,_,dayOfWeek) <- fmap (toWeekDate . localDay . zonedTimeToLocalTime) .
                    utcToLocalZonedTime =<< getCurrentTime
  menu <- getMenu' opts
  if dayOfWeek >= 6
    then putStrLn "It's weekend!"
    else case M.lookup (toEnum dayOfWeek) menu of
           Just daymenu -> putStrLn (ppDayMenu daymenu)
                   | date opts == Week = putStrLn . ppWeekMenu =<< getMenu' opts

ppWeekMenu = unlines . concat . M.elems . M.mapWithKey (\day dm -> [show day, ppDayMenu dm])

ppDayMenu :: DayMenu -> String
ppDayMenu = unlines . map showMeal . listify isMeal
  where isMeal _ = True
        showMeal (Meal d t) = printf "[%s] - %s" (show t) d

main = handleCommand =<< cmdArgs opts
