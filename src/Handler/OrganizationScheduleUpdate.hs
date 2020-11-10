{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.OrganizationScheduleUpdate where

import qualified Database.Persist        as P
import           Import
import qualified Rentier.FormField
import           Rentier.HumanSelectable
import           Rentier.SimpleForm
import           Yesod.Form.Bootstrap3

getOrganizationScheduleUpdateR :: MerchantId -> OrganizationId -> OrganizationScheduleId -> Handler Html
getOrganizationScheduleUpdateR merchantId organizationId organizationScheduleId = do
  mi <- runDB $ get organizationId
  mm <- runDB $ get organizationScheduleId
  case (mi, mm) of
    (Just ident, Just oldM) -> do
      (formWidget, formEnctype) <- generateFormPost $
                                   renderBootstrap3 BootstrapBasicForm
                                   (aForm organizationId oldM)
      renderSimpleForm formWidget formEnctype (formSettings merchantId organizationId organizationScheduleId ident)
    (_, _) ->
      permissionDenied ""


postOrganizationScheduleUpdateR :: MerchantId -> OrganizationId -> OrganizationScheduleId -> Handler Html
postOrganizationScheduleUpdateR merchantId organizationId organizationScheduleId = do
  mi <- runDB $ get organizationId
  mm <- runDB $ get organizationScheduleId
  case (mi, mm) of
    (Just ident, Just oldM) -> do
      ((formResult, formWidget), formEnctype) <- runFormPost $
                                                 renderBootstrap3 BootstrapBasicForm
                                                 (aForm organizationId oldM)
      case formResult of
          FormSuccess formSuccess -> do
            _ <- runDB $ P.update organizationScheduleId [
                OrganizationScheduleWeekDays P.=. organizationScheduleWeekDays formSuccess,
                OrganizationScheduleStartsAt P.=. organizationScheduleStartsAt formSuccess,
                OrganizationScheduleEndsAt P.=. organizationScheduleEndsAt formSuccess,
                OrganizationScheduleMinimalRentDuration P.=. organizationScheduleMinimalRentDuration formSuccess,
                OrganizationScheduleTimeKind P.=. organizationScheduleTimeKind formSuccess
              ]
            setMessageI MsgOrganizationScheduleUpdated
            redirect $ OrganizationUpdateR merchantId organizationId
          _ ->
            renderSimpleForm formWidget formEnctype (formSettings merchantId organizationId organizationScheduleId ident)
    (_, _) ->
      permissionDenied ""

aForm :: OrganizationId -> OrganizationSchedule -> AForm Handler OrganizationSchedule
aForm organizationId oldM = OrganizationSchedule
    <$> areq hiddenField (bfs MsgNothing) (Just organizationId)
    <*> areq (checkboxesFieldList selectOptionsList) (bfs MsgWeekDay) (Just $ organizationScheduleWeekDays oldM)
    <*> areq Rentier.FormField.roundedTimeField (bfs MsgStartsAt) (Just $ organizationScheduleStartsAt oldM)
    <*> areq Rentier.FormField.roundedTimeField (bfs MsgEndsAt) (Just $ organizationScheduleEndsAt oldM)
    <*> areq Rentier.FormField.posTimeField (bfs MsgMinimalRentDuration) (Just $ organizationScheduleMinimalRentDuration oldM)
    <*> areq (selectFieldList selectOptionsList) (bfs MsgTimeKind) (Just $ organizationScheduleTimeKind oldM)

formSettings :: MerchantId -> OrganizationId -> OrganizationScheduleId -> Organization -> SimpleFormSettings
formSettings merchantId organizationId organizationScheduleId org =
  SimpleFormSettings{
    formRoute = OrganizationScheduleUpdateR merchantId organizationId organizationScheduleId,
    formMsgSubmit = MsgOrganizationScheduleUpdate,
    formPageTitle = MsgOrganizationScheduleUpdateRTitle $ organizationName org,
    formLayout = defaultLayout
  }
