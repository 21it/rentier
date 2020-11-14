{-# LANGUAGE RecordWildCards #-}

module Rentier.FullCalendar where

import Import

data FullCalendarEvent
  = FullCalendarEvent
      { fullCalendarEventStart :: UTCTime,
        fullCalendarEventEnd :: UTCTime
      }
  deriving (Show, Eq)

instance ToJSON FullCalendarEvent where
  toJSON FullCalendarEvent {..} =
    object
      [ "start" .= fullCalendarEventStart,
        "end" .= fullCalendarEventEnd,
        "title"
          .= ( format fullCalendarEventStart
                 <> " -\n"
                 <> format fullCalendarEventEnd
             )
      ]
    where
      format = formatTime defaultTimeLocale "%R"
