{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Rentier.HumanReadable where

import           Import
import           Rentier.EmployeeInvitationStatus
import           Rentier.Property
import           Rentier.Time
import           Rentier.WeekDay
import           RIO.Time

class HumanReadable a where
  toMsg :: a -> AppMessage
  toShortMsg :: a -> AppMessage
  toShortMsg = toMsg
  toText :: App -> [Text] -> a -> Text
  toText app langs x = renderMessage app langs (toMsg x)
  toShortText :: App -> [Text] -> a -> Text
  toShortText app langs x = renderMessage app langs (toShortMsg x)

instance HumanReadable AppMessage where
  toMsg x = x

instance HumanReadable TimeKind where
  toMsg WorkingTime = MsgWorkingTime
  toMsg BreakTime   = MsgBreakTime

instance HumanReadable PropertyKind where
  toMsg Primary   = MsgPrimary
  toMsg Secondary = MsgSecondary

instance HumanReadable PropertyAvailability where
  toMsg InOrganization = MsgInOrganization
  toMsg Anytime        = MsgAnytime

instance HumanReadable TimeOfDay where
  toMsg x = MsgProxy $ pack $ formatTime defaultTimeLocale "%R" x

instance HumanReadable WeekDay where
  toMsg Sunday    = MsgSunday
  toMsg Monday    = MsgMonday
  toMsg Tuesday   = MsgTuesday
  toMsg Wednesday = MsgWednesday
  toMsg Thursday  = MsgThursday
  toMsg Friday    = MsgFriday
  toMsg Saturday  = MsgSaturday

  toShortMsg Sunday    = MsgShortSunday
  toShortMsg Monday    = MsgShortMonday
  toShortMsg Tuesday   = MsgShortTuesday
  toShortMsg Wednesday = MsgShortWednesday
  toShortMsg Thursday  = MsgShortThursday
  toShortMsg Friday    = MsgShortFriday
  toShortMsg Saturday  = MsgShortSaturday

instance Show a => HumanReadable (Maybe a) where
  toMsg Nothing  = MsgProxy ""
  toMsg (Just a) = (MsgProxy . pack . show) a

instance HumanReadable Bool where
  toMsg True  = MsgYes
  toMsg False = MsgNo

instance HumanReadable EmployeeInvitationStatus where
  toMsg Pending  = MsgEmployeeInvitationStatusPending
  toMsg Accepted = MsgEmployeeInvitationStatusAccepted
  toMsg Declined = MsgEmployeeInvitationStatusDeclined

--
--  TODO : use TodRange instead of tuple!
--

instance HumanReadable (TimeOfDay, TimeOfDay) where
  toMsg (a, b) = MsgProxy $ pack $ showTime a ++ " - " ++ showTime b
    where
      showTime = formatTime defaultTimeLocale "%R"
