{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Rentier.Session where

import           Data.Time.Clock
import           Import
import qualified Rentier.Storage as Storage

createSessionHandler :: PropertyId
                        -> UTCTime
                        -> UTCTime
                        -> (Organization -> Property -> [Entity Property] -> Handler Html)
                        -> Handler Html
createSessionHandler propertyId startsAt endsAt renderer = do
  po <- Storage.fetchPrimaryPropertyAndOrganization propertyId
  case po of
    Just (Entity{entityVal = mainProperty}, Entity{entityVal = org, entityKey = organizationId}) -> do
      ps <- Storage.fetchAvailableSecondaryProperties organizationId startsAt endsAt
      renderer org mainProperty ps
    Nothing ->
      permissionDenied ""
