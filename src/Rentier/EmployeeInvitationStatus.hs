module Rentier.EmployeeInvitationStatus where

import Data.Text
import Database.Persist.TH
import Import.External
import Yesod.Core.Dispatch

data EmployeeInvitationStatus
  = Pending
  | Accepted
  | Declined
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "EmployeeInvitationStatus"

instance PathPiece EmployeeInvitationStatus where
  fromPathPiece :: Text -> Maybe EmployeeInvitationStatus
  fromPathPiece qs = readMaybe $ unpack $ toTitle qs

  toPathPiece :: EmployeeInvitationStatus -> Text
  toPathPiece code = toLower $ pack $ show code
