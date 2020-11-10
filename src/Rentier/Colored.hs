{-# LANGUAGE NoImplicitPrelude #-}

module Rentier.Colored where

import           Import
import           Rentier.BootstrapColor
import           Rentier.EmployeeInvitationStatus
import           Rentier.Time

class Colored a where
  color :: a -> Maybe BootstrapColor

instance Colored OrganizationSchedule where
  color x =
    case organizationScheduleTimeKind x of
      WorkingTime -> Just Success
      BreakTime   -> Just Danger

instance Colored Property where
  color x =
    if propertyActive x
      then Just Success
      else Just Danger

instance Colored Organization where
  color x =
    if organizationActive x
      then Just Success
      else Just Danger

instance Colored Employee where
  color x =
    case employeeInvitationStatus x of
      Pending  -> Just Warning
      Accepted -> if employeeActive x then Just Success else Just Danger
      Declined -> Nothing
