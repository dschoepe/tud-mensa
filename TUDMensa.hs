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
import Data.List
import Data.Time
import Data.Time.Calendar.WeekDate
import Text.Printf
import qualified Data.Map as M

data Options = Options { date :: Date
                       , hide :: [MealType]
                         -- ^ types of values to hide
                       , location :: Location
                         -- ^ location of the canteen
                       , printDayMenu :: DayMenu -> String
                         -- ^ function to print daily menus
                       , printWeekMenu :: WeekMenu -> String
                         -- ^ function to print weekly menus
                       , filterMeals :: WeekMenu -> IO WeekMenu
                         -- ^ custom filtering function for meals
                       , showAll :: Bool
                         -- ^ show all meals, regardless of hide and filterMeals
                       } deriving (Data, Typeable)

-- | Annotates options for use with cmdargs
annotateOpts :: Options -> Options
annotateOpts opts@Options{..} =
  opts { date = enum . map annotateDate $ date : ([NextWeek .. Today] \\ [date])
       , hide = hide &= typ "Type of Meal"
                &= help hideHelp
       , location = location &= help locationHelp
       , printDayMenu = printDayMenu &= ignore
       , printWeekMenu = printWeekMenu &= ignore
       , filterMeals = filterMeals &= ignore
       , showAll = showAll &= help showAllHelp
                   &= explicit &= name "all" &= name "a"
       }
  &= program "mensa" &= summary "tud-mensa 0.1"
  &= details ["http://github.com/dschoepe/tud-mensa"]
    where dateHelp = "Show menu for entire week or just today"
          hideHelp = "Don't show these types of food in the result. If "++
                     "specified multiple times all specified types will be hidden."
          locationHelp = "Specify where you are."
          showAllHelp = "Ignore filtering options. This is only useful if "++
                        "you set default filtering options in your config file."
          annotateDate Today = Today &= help "Show menu for today"
          annotateDate ThisWeek = ThisWeek &= explicit &= name "week"
                                  &= help "Show menu for this week" &= name "w"
          annotateDate NextWeek = NextWeek &= help "Show menu for next week"

defaultOpts :: Options
defaultOpts = Options { date = Today
                      , hide = []
                      , location = Stadtmitte
                      , printDayMenu = ppDayMenu
                      , printWeekMenu = ppWeekMenu
                      , filterMeals = return
                      , showAll = False
                      }

-- | Retrieve and parse menu for current week
getMenu :: Date -> Location -> IO WeekMenu
getMenu = (.) (fmap parseWeek) . getWeekly

-- | Filter the menu according to given options
filterByOpts :: Options -> WeekMenu -> IO WeekMenu
filterByOpts (Options{..})
  | showAll = return
  | otherwise = filterMeals . everywhere (mkT $ filter byType)
  where byType = (`notElem` hide) . mealType

-- | Filter menu according to options
getMenuFiltered :: Options -> IO WeekMenu
getMenuFiltered opts = filterByOpts opts =<< getMenu (date opts) (location opts)

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
  | date `elem` [ThisWeek, NextWeek] = putStr . printWeekMenu =<<
                                       getMenuFiltered opts

-- | Default pretty-printer for week menu
ppWeekMenu :: WeekMenu -> String
ppWeekMenu = unlines . concat . M.elems .
             M.mapWithKey (\day dm -> [show day, ppDayMenu dm])

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

