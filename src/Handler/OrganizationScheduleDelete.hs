{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.OrganizationScheduleDelete where

import qualified Database.Persist as P
import           Import

getOrganizationScheduleDeleteR :: MerchantId -> OrganizationId -> OrganizationScheduleId -> Handler Html
getOrganizationScheduleDeleteR merchantId organizationId organizationScheduleId = do
  mm <- runDB $ get organizationScheduleId
  case mm of
    Just _ -> do
      _ <- runDB $ P.delete organizationScheduleId
      setMessageI MsgOrganizationScheduleDeleted
      redirect $ OrganizationUpdateR merchantId organizationId
    Nothing ->
      permissionDenied ""
