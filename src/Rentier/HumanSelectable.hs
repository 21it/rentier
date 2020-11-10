{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE StandaloneDeriving      #-}

module Rentier.HumanSelectable where

import           Import
import           Rentier.HumanReadable
import           Rentier.Property
import           Rentier.Time
import           Rentier.WeekDay

class HumanSelectable a where
  selectOptionsList :: (HumanReadable a, Enum a, Bounded a) => [(AppMessage, a)]
  selectOptionsList = map (\x -> (toMsg x, x)) [minBound..maxBound]

deriving instance HumanSelectable TimeKind
deriving instance HumanSelectable PropertyKind
deriving instance HumanSelectable PropertyAvailability
deriving instance HumanSelectable WeekDay
