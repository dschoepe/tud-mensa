{-# LANGUAGE DeriveDataTypeable #-}
module TUDMensa.Types where

import qualified Data.Map as M
import Data.Generics

data Meal = Meal { dish ::  String
                 , mealType :: MealType } deriving (Show, Eq, Ord, Data, Typeable)

data MealType = Poultry | Pork | Beef | BeefAndPork | Fish | Vegetarian |
                Lamb | Venison | Unknown
              deriving (Show, Eq, Ord, Data, Typeable, Enum)

data DayMenu = DayMenu { bistro :: [Meal]
                       , soup :: [Meal]
                       , gabel :: [Meal]
                       , wok :: [Meal]
                       , ottob :: [Meal]
                       } deriving (Data, Typeable, Show, Ord, Eq)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday
         deriving (Ord, Show, Enum, Eq, Data, Typeable)

type WeekMenu = M.Map Day DayMenu

data Date = NextWeek | ThisWeek | Today deriving (Data, Typeable, Show, Eq, Enum)
