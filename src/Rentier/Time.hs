{-# LANGUAGE InstanceSigs #-}

module Rentier.Time where

import Data.Text
import Data.Time.ISO8601
import Data.Time.LocalTime
import Database.Persist.TH
import Import.External
import RIO.Time

data TimeKind = WorkingTime | BreakTime deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "TimeKind"

type Minutes = Int

data MinutesRange = MinutesRange Minutes Minutes

newtype UTC = UTC UTCTime deriving (Show, Read, Eq)

tod2mins :: TimeOfDay -> Minutes
tod2mins t = 60 * todHour t + todMin t

tod2minsRange :: TimeOfDay -> TimeOfDay -> MinutesRange
tod2minsRange from to =
  if minsFrom < minsTo
    then MinutesRange minsFrom minsTo
    else MinutesRange minsFrom (minsTo + minsPerDay)
  where
    minsFrom = tod2mins from
    minsTo = tod2mins to

minsRangeDayShift :: MinutesRange -> MinutesRange
minsRangeDayShift (MinutesRange from to) =
  MinutesRange (from + minsPerDay) (to + minsPerDay)

minsPerDay :: Minutes
minsPerDay = 1440

createUTCTime :: Day -> Minutes -> UTCTime
createUTCTime d m =
  UTCTime
    { utctDay = addDays (toInteger $ div m minsPerDay) d,
      utctDayTime = secondsToDiffTime $ toInteger (mod m minsPerDay) * 60
    }

todFromUTCTime :: UTCTime -> TimeOfDay
todFromUTCTime = localTimeOfDay . utcToLocalTime utc

instance PathPiece UTC where
  fromPathPiece :: Text -> Maybe UTC
  fromPathPiece x = parseISO8601 (unpack x) >>= Just . UTC
  toPathPiece :: UTC -> Text
  toPathPiece (UTC x) = pack $ formatISO8601 x
