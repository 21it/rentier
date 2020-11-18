{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import ClassyPrelude.Yesod
import Data.Time.LocalTime (TimeOfDay)
import Database.Persist.Quasi
import Import.External (coerce)
import qualified Rentier.Currency
import Rentier.Data.Type
import Rentier.EmployeeInvitationStatus
import Rentier.Property
import Rentier.SessionStatus
import qualified Rentier.Time

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

instance ToMessage User where
  toMessage = coerce . userIdent
