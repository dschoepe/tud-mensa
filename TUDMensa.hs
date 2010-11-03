{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module TUDMensa (defaultOpts, ppDayMenu, ppWeekMenu,
                 tudMensa, getMenu, getMenuFiltered,
                 Options(..),
                 module TUDMensa.Types)
       where

import TUDMensa.Parse
import TUDMensa.Get
import TUDMensa.Types

import Control.Monad
import qualified Config.Dyre as Dyre
import System.Console.CmdArgs
import Data.Generics
import Data.Time
import Data.Time.Calendar.WeekDate
import Text.Printf
import qualified Data.Map as M

data Date = Week | Today deriving (Data, Typeable, Show, Eq)
data Options = Options { date :: Date
                       , hide :: [MealType] -- ^ types of values to hide
                       , printDayMenu :: DayMenu -> String -- ^ function to print daily menus
                       , printWeekMenu :: WeekMenu -> String -- ^ function to print weekly menus
                       , filterMeals :: WeekMenu -> IO WeekMenu -- ^ custom filtering function for meals
                       , showAll :: Bool -- ^ show all meals, regardless of hide and filterMeals
                       } deriving (Data, Typeable)

-- | Annotates options for use with cmdargs
annotateOpts :: Options -> Options
annotateOpts opts@Options{..} =
  opts { date = let today = Today &= help "Show menu for today"
                    week = Week &= help "Show menu for entire week"
                in case date of
                  Today -> enum [today, week]
                  _ -> enum [week, today]
       , hide = hide &= typ "Type of Meal"
                &= help hideHelp
       , printDayMenu = printDayMenu &= ignore
       , printWeekMenu = printWeekMenu &= ignore
       , filterMeals = filterMeals &= ignore
       , showAll = showAll &= help showAllHelp &= explicit &= name "all"
       }
  &= program "mensa" &= summary "tud-mensa 0.1"
    where dateHelp = "Show menu for entire week or just today"
          hideHelp = "Don't show these types of food in the result. "++
                     "If specified multiple times all specified types will be hidden."
          showAllHelp = "Ignore filtering options. This is only useful if you set default "++
                        "filtering options in your config file."
defaultOpts :: Options
defaultOpts = Options { date = Today
                      , hide = []
                      , printDayMenu = ppDayMenu
                      , printWeekMenu = ppWeekMenu
                      , filterMeals = return
                      , showAll = False
                      }

-- | Retrieve and parse menu for current week
getMenu :: IO WeekMenu
getMenu = parseWeek `fmap` getWeekly

-- | Filter the menu according to given options
filterByOpts :: Options -> WeekMenu -> IO WeekMenu
filterByOpts (Options{..})
  | showAll = return
  | otherwise = filterMeals . everywhere (mkT $ filter byType)
  where byType = (`notElem` hide) . mealType

-- | Filter menu according to options
getMenuFiltered :: Options -> IO WeekMenu
getMenuFiltered opts = filterByOpts opts =<< getMenu

handleCommand :: Options -> IO ()
handleCommand opts@(Options{..})
  | date == Today = do
  (_,_,dayOfWeek) <- fmap (toWeekDate . localDay . zonedTimeToLocalTime) .
                    utcToLocalZonedTime =<< getCurrentTime
  menu <- getMenuFiltered opts
  if dayOfWeek >= 6
    then putStrLn "It's weekend! No crappy cafeteria food today."
    else case M.lookup (toEnum (dayOfWeek - 1)) menu of
           Just daymenu -> putStr (printDayMenu daymenu)
  | date == Week = putStr . printWeekMenu =<< getMenuFiltered opts

-- | Default pretty-printer for week menu
ppWeekMenu :: WeekMenu -> String
ppWeekMenu = unlines . concat . M.elems . M.mapWithKey (\day dm -> [show day, ppDayMenu dm])

-- | Default pretty-printer for day menu
ppDayMenu :: DayMenu -> String
ppDayMenu = unlines . map showMeal . listify isMeal
  where isMeal _ = True
        showMeal (Meal d t) = printf "[%s] - %s" (show t) d

realMain cfg = cmdArgs (annotateOpts cfg) >>= handleCommand

tudMensa = Dyre.wrapMain $ Dyre.defaultParams
           { Dyre.projectName = "tud-mensa"
           , Dyre.realMain = realMain
           , Dyre.showError = \_ err -> defaultOpts
           }

