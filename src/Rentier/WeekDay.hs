{-# LANGUAGE TemplateHaskell #-}
module Rentier.WeekDay where

import           Database.Persist.TH

data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "WeekDay"
