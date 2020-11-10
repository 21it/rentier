{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.PropertyUpdate where

import qualified Database.Persist        as P
import           Import
import           Rentier.FormField
import           Rentier.HumanSelectable
import           Rentier.SimpleForm
import           Yesod.Form.Bootstrap3

getPropertyUpdateR :: MerchantId -> OrganizationId -> PropertyId -> Handler Html
getPropertyUpdateR merchantId organizationId propertyId = do
  mp <- runDB $ get organizationId
  mm <- runDB $ get propertyId
  case (mp, mm) of
    (Just parent, Just oldM) -> do
      (formWidget, formEnctype) <- generateFormPost $
                                   renderBootstrap3 BootstrapBasicForm
                                   (aForm organizationId propertyId parent oldM)
      renderSimpleForm formWidget formEnctype (formSettings merchantId organizationId propertyId oldM)
    (_, _) ->
      permissionDenied ""


postPropertyUpdateR :: MerchantId -> OrganizationId -> PropertyId -> Handler Html
postPropertyUpdateR merchantId organizationId propertyId = do
  mp <- runDB $ get organizationId
  mm <- runDB $ get propertyId
  case (mp, mm) of
    (Just parent, Just oldM) -> do
      ((formResult, formWidget), formEnctype) <- runFormPost $
                                                 renderBootstrap3 BootstrapBasicForm
                                                 (aForm organizationId propertyId parent oldM)
      case formResult of
          FormSuccess formSuccess -> do
            _ <- runDB $ P.update propertyId [
                PropertyName P.=. propertyName formSuccess,
                PropertyKind P.=. propertyKind formSuccess,
                PropertyQty P.=. propertyQty formSuccess,
                PropertyBasePrice P.=. propertyBasePrice formSuccess,
                PropertyBaseRentDuration P.=. propertyBaseRentDuration formSuccess,
                PropertyIframeTimeResolution P.=. propertyIframeTimeResolution formSuccess,
                PropertyActive P.=. propertyActive formSuccess
              ]
            setMessageI MsgPropertyUpdated
            redirect $ OrganizationUpdateR merchantId organizationId
          _ ->
            renderSimpleForm formWidget formEnctype (formSettings merchantId organizationId propertyId oldM)
    (_, _) ->
      permissionDenied ""

aForm :: OrganizationId -> PropertyId -> Organization -> Property -> AForm Handler Property
aForm organizationId propertyId org oldM = Property
    <$> areq propertyNameField (bfs MsgNameObject) (Just $ propertyName oldM)
    <*> areq hiddenField (bfs MsgNothing) (Just organizationId)
    <*> areq (selectFieldList selectOptionsList) (bfs MsgKind) (Just $ propertyKind oldM)
    <*> areq (selectFieldList selectOptionsList) (bfs MsgAvailability) (Just $ propertyAvailability oldM)
    <*> aopt Rentier.FormField.natIntField (bfs MsgQty) (Just $ propertyQty oldM)
    <*> areq Rentier.FormField.nonNegRatField (bfs $ MsgBasePrice $ organizationBaseCurrencyCode org) (Just $ propertyBasePrice oldM)
    <*> areq posTimeField (bfs MsgBaseRentDuration) (Just $ propertyBaseRentDuration oldM)
    <*> areq posTimeField (bfs MsgIframeTimeResolution) (Just $ propertyIframeTimeResolution oldM)
    <*> areq checkBoxField (bfs MsgActive) (Just $ propertyActive oldM)
    <*> areq hiddenField (bfs MsgNothing) (Just $ propertyExists oldM)
    where
      propertyNameField = checkM (validatePropertyName organizationId propertyId) textField

validatePropertyName :: OrganizationId -> PropertyId -> Text -> Handler (Either AppMessage Text)
validatePropertyName organizationId propertyId name = do
  mm <- runDB $ getBy $ UniqueProperty name organizationId
  return $ case mm of
    Just Entity{entityKey = thisId} | thisId /= propertyId -> Left MsgAlreadyExists
    Just Entity{} -> Right name
    Nothing -> Right name
--
formSettings :: MerchantId -> OrganizationId -> PropertyId -> Property -> SimpleFormSettings
formSettings merchantId organizationId propertyId ident =
  SimpleFormSettings{
    formRoute = PropertyUpdateR merchantId organizationId propertyId,
    formMsgSubmit = MsgPropertyUpdate,
    formPageTitle = MsgPropertyUpdateRTitle $ propertyName ident,
    formLayout = defaultLayout
  }
