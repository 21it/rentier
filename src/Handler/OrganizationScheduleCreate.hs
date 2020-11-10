{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.OrganizationScheduleCreate where

import           Import
import           Rentier.FormField
import           Rentier.HumanSelectable
import           Rentier.SimpleForm
import           Rentier.Time
import           Rentier.Utils
import           Yesod.Form.Bootstrap3

getOrganizationScheduleCreateR :: MerchantId -> OrganizationId -> Handler Html
getOrganizationScheduleCreateR merchantId organizationId = do
  morg <- runDB $ get organizationId
  case morg of
    Just org -> do
      (formWidget, formEnctype) <- generateFormPost $
                                   renderBootstrap3 BootstrapBasicForm
                                   (aForm organizationId org)
      renderSimpleForm formWidget formEnctype (formSettings merchantId organizationId org)
    Nothing ->
      permissionDenied ""

postOrganizationScheduleCreateR :: MerchantId -> OrganizationId -> Handler Html
postOrganizationScheduleCreateR merchantId organizationId = do
  morg <- runDB $ get organizationId
  case morg of
    Just org -> do
      ((formResult, formWidget), formEnctype) <- runFormPost $
                                                 renderBootstrap3 BootstrapBasicForm
                                                 (aForm organizationId org)
      case formResult of
          FormSuccess formSuccess -> do
            _ <- runDB $ insert400 formSuccess{
              organizationScheduleOrganizationId = organizationId
            }
            setMessageI MsgOrganizationScheduleCreated
            redirect $ OrganizationUpdateR merchantId organizationId
          _ ->
            renderSimpleForm formWidget formEnctype (formSettings merchantId organizationId org)
    Nothing ->
      permissionDenied ""

aForm :: OrganizationId -> Organization -> AForm Handler OrganizationSchedule
aForm organizationId org = OrganizationSchedule
    <$> areq hiddenField (bfs MsgNothing) (Just organizationId)
    <*> areq (checkboxesFieldList selectOptionsList) (bfsAutoFocus MsgWeekDay) Nothing
    <*> areq roundedTimeField (bfs MsgStartsAt) Nothing
    <*> areq roundedTimeField (bfs MsgEndsAt) Nothing
    <*> areq posTimeField (bfs MsgMinimalRentDuration) (Just $ organizationDefaultMinimalRentDuration org)
    <*> areq (selectFieldList selectOptionsList) (bfs MsgTimeKind) (Just WorkingTime)

formSettings :: MerchantId -> OrganizationId -> Organization -> SimpleFormSettings
formSettings merchantId organizationId org =
  SimpleFormSettings{
    formRoute = OrganizationScheduleCreateR merchantId organizationId,
    formMsgSubmit = MsgOrganizationScheduleCreate,
    formPageTitle = MsgOrganizationScheduleCreateRTitle $ organizationName org,
    formLayout = defaultLayout
  }
