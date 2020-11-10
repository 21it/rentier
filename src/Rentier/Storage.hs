{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Rentier.Storage where

import qualified Data.List
import qualified Data.Map.Strict  as Map
import           Data.Time.Clock
import qualified Database.Persist as P
import           Import
import           Rentier.Property


ensureCustomer :: OrganizationId -> Handler CustomerId
ensureCustomer organizationId = do
  (userId, _) <- requireAuthPair
  mc <- runDB $ getBy $ UniqueCustomer organizationId userId
  case mc of
    Just c -> return $ entityKey c
    Nothing -> runDB $ insert400 Customer{
                  customerOrganizationId = organizationId,
                  customerUserId = userId,
                  customerBalance = 0
                }

fetchPrimaryPropertyAndOrganization :: PropertyId -> Handler (Maybe (Entity Property, Entity Organization))
fetchPrimaryPropertyAndOrganization propertyId = do
  po <- runDB $ select $
        from $ \(p, o) -> do
          where_ (p ^. PropertyId ==. val propertyId)
          where_ (p ^. PropertyOrganizationId ==. o ^. OrganizationId)
          return (p, o)
  return $ case po of
    [(p, o)] -> if propertyKind (entityVal p) == Primary then Just (p, o) else Nothing
    _ -> Nothing

fetchAvailableSecondaryProperties :: OrganizationId -> UTCTime -> UTCTime -> Handler [Entity Property]
fetchAvailableSecondaryProperties organizationId startsAt endsAt = do
  sps <- runDB $ select $
          from $ \(s, sp, p) -> do
            where_ (p ^. PropertyOrganizationId ==. val organizationId)
            where_
              (
                (
                  (s ^. SessionStartsAt >=. val startsAt)
                  &&.
                  (s ^. SessionStartsAt <. val endsAt)
                )
                ||.
                (
                  (s ^. SessionEndsAt >. val startsAt)
                  &&.
                  (s ^. SessionEndsAt <=. val endsAt)
                )
                ||.
                (
                  (s ^. SessionStartsAt <=. val startsAt)
                  &&.
                  (s ^. SessionEndsAt >=. val endsAt)
                )
              )
            where_ (sp ^. SessionPropertySessionId ==. s ^. SessionId)
            where_ (sp ^. SessionPropertyPropertyId ==. p ^. PropertyId)
            return sp
  let qtyDict = Data.List.foldl
                 (\acc SessionProperty{sessionPropertyPropertyId = p, sessionPropertyQty = qty} ->
                   if Map.member p acc
                   then Map.update (\x -> Just $ x + qty) p acc
                   else Map.insert p qty acc
                 )
                 Map.empty
                 (entityVal <$> sps)
  ps <- runDB $ selectList [PropertyOrganizationId P.==. organizationId, PropertyKind P.==. Secondary] []
  return $ ps >>= (\p ->
      let
        k = entityKey p
        v = entityVal p
      in
      case (-) <$> propertyQty v <*> Map.lookup k qtyDict of
        Nothing            -> [p]
        Just qty | qty > 0 -> [p{entityVal = v{propertyQty = Just qty}}]
        Just _             -> []
    )
