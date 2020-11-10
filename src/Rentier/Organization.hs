module Rentier.Organization where

import           Data.Range.Range
import           Model
import           Rentier.Enum
import           Rentier.Time
import           Rentier.WeekDay

schedule2Ranges :: WeekDay -> [OrganizationSchedule] -> [MinutesRange]
schedule2Ranges thisWd xs =
  mergeRanges (boundaries ++ difference pos neg)
  >>= spanRangeOnly
  >>= (\mr -> foldl (splitRange mr) [] thisXs)
  where
    onlyPosRules = filter (\x -> organizationScheduleTimeKind x == WorkingTime)
    onlyNegRules = filter (\x -> organizationScheduleTimeKind x /= WorkingTime)
    parseRange x =
      let
        MinutesRange from to = tod2minsRange (organizationScheduleStartsAt x) (organizationScheduleEndsAt x)
        in SpanRange from to
    reParseRange (MinutesRange from to) = SpanRange from to
    spanRangeOnly x =
      case x of
        SpanRange from to -> [MinutesRange from to]
        _                 -> []
    nextWd = next thisWd
    thisXs = filter (elem thisWd . organizationScheduleWeekDays) xs
    nextXs = filter (elem nextWd . organizationScheduleWeekDays) xs
    pos = mergeRanges $ map parseRange (onlyPosRules thisXs)
    thisNeg = map parseRange (onlyNegRules thisXs)
    nextNeg = map (reParseRange . minsRangeDayShift) (map parseRange (onlyNegRules nextXs) >>= spanRangeOnly)
    neg = mergeRanges $ thisNeg ++ nextNeg
    boundaries = intersection pos neg >>= \r ->
      case r of
        SingletonRange _ -> [r]
        SpanRange x y    -> [SingletonRange x, SingletonRange y]
        _                -> []
    splitRange :: MinutesRange -> [MinutesRange] -> OrganizationSchedule -> [MinutesRange]
    splitRange mr acc os =
      case mergeRanges $ intersection [sr] [sr'] of
        [SpanRange from to] ->
          case divMod (to - from) dm of
            (0, _) ->
              mr:acc
            (x, y) ->
              let
                chunks = (\n -> MinutesRange (from + n * dm) (from + (n + 1) * dm)) <$> [0..x-1]
                in case y of
                  0 -> chunks ++ acc
                  _ -> MinutesRange (from + x * dm) to : chunks ++ acc
        _ ->
          acc
      where
        MinutesRange f t = mr
        dm = (tod2mins . organizationScheduleMinimalRentDuration) os
        MinutesRange f' t' = tod2minsRange (organizationScheduleStartsAt os) (organizationScheduleEndsAt os)
        sr = SpanRange f t
        sr' = SpanRange f' t'
