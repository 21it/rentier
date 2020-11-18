{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.IframePropertySchedule where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.ISO8601
import qualified Database.Persist as P
import Import
import Rentier.FullCalendar
import Rentier.HumanReadable
import Rentier.Organization
import Rentier.Property
import Rentier.Time

getIframePropertyScheduleR :: PropertyId -> Handler TypedContent
getIframePropertyScheduleR propertyId = do
  po <- runDB $ select
    $ from
    $ \(p, o) -> do
      where_ (p ^. PropertyId ==. val propertyId)
      where_ (p ^. PropertyOrganizationId ==. o ^. OrganizationId)
      return (p, o)
  case po of
    [(Entity {entityVal = Property {propertyKind = Primary}}, Entity {entityVal = org, entityKey = organizationId})] ->
      selectRep $ do
        provideRep $ do
          --
          --  TODO : maybe improve this in other places (replace app/langs)
          --
          hr2Text <- getMessageRender
          noLayout $ do
            let iframeTimeResolution =
                  toMsg $
                    organizationDefaultIframeTimeResolution org
            setTitleI
              MsgHomeRTitle
            addStylesheet $
              StaticR fullcalendar_4_1_0_packages_core_main_css
            addStylesheet $
              StaticR fullcalendar_4_1_0_packages_daygrid_main_css
            addStylesheet $
              StaticR fullcalendar_4_1_0_packages_timegrid_main_css
            addScript $
              StaticR fullcalendar_4_1_0_packages_core_main_js
            addScript $
              StaticR fullcalendar_4_1_0_packages_daygrid_main_js
            addScript $
              StaticR fullcalendar_4_1_0_packages_timegrid_main_js
            $(widgetFile "iframe_property_schedule")
        provideRep $ do
          --
          -- TODO : fix this unsafe code, remove "start" and "end" hardcoded parameters
          --
          ds <- liftMaybe =<< fmap (\x -> x >>= parseISO8601 . unpack >>= Just . utctDay) (lookupGetParam "start")
          de <- liftMaybe =<< fmap (\x -> x >>= parseISO8601 . unpack >>= Just . utctDay) (lookupGetParam "end")
          scheduleList <- fmap (map entityVal) (runDB $ selectList [OrganizationScheduleOrganizationId P.==. organizationId] [])
          let eventsList = [ds .. de] >>= \d -> map (createEvent d) (schedule2Ranges (day2WeekDay d) scheduleList)
          return $ toEncoding eventsList
      where
        day2WeekDay :: Day -> WeekDay
        day2WeekDay x =
          toEnum $ intWeekDay - 1
          where
            (_, _, intWeekDay) = toWeekDate x
        createEvent :: Day -> MinutesRange -> FullCalendarEvent
        createEvent d (MinutesRange ms me) =
          FullCalendarEvent
            { fullCalendarEventStart = createUTCTime d ms,
              fullCalendarEventEnd = createUTCTime d me
            }
    _ ->
      permissionDenied ""

liftMaybe :: Maybe a -> Handler a
liftMaybe = \case
  Nothing -> permissionDenied ""
  Just x -> return x
