{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}
module Rentier.SessionStatus where

import           Data.Text
import           Database.Persist.TH
import           Text.Read
import           Yesod.Core.Dispatch

data SessionStatus = Reserved |
                     CancelledSoft |
                     CancelledHard |
                     Ignored |
                     AwaitingForPayment |
                     Payed
                     deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "SessionStatus"

instance PathPiece SessionStatus where
  fromPathPiece :: Text -> Maybe SessionStatus
  fromPathPiece = readMaybe . unpack . toTitle

  toPathPiece :: SessionStatus -> Text
  toPathPiece = toLower . pack . show
