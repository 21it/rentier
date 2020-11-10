{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.PropertyCreate where

import           Import
import           Rentier.FormField
import           Rentier.HumanSelectable
import           Rentier.SimpleForm
import           Rentier.Utils
import           Yesod.Form.Bootstrap3

getPropertyCreateR :: MerchantId -> OrganizationId -> Handler Html
getPropertyCreateR merchantId organizationId = do
  morg <- runDB $ get organizationId
  case morg of
    Just org -> do
      (formWidget, formEnctype) <- generateFormPost $
                                   renderBootstrap3 BootstrapBasicForm
                                   (aForm organizationId org)
      renderSimpleForm formWidget formEnctype (formSettings merchantId organizationId org)
    Nothing ->
      permissionDenied ""

postPropertyCreateR :: MerchantId -> OrganizationId -> Handler Html
postPropertyCreateR merchantId organizationId = do
  morg <- runDB $ get organizationId
  case morg of
    Just org -> do
      ((formResult, formWidget), formEnctype) <- runFormPost $
                                                 renderBootstrap3 BootstrapBasicForm
                                                 (aForm organizationId org)
      case formResult of
          FormSuccess formSuccess -> do
            _ <- runDB $ insert400 formSuccess{
              propertyOrganizationId = organizationId
            }
            setMessageI MsgPropertyCreated
            redirect $ OrganizationUpdateR merchantId organizationId
          _ ->
            renderSimpleForm formWidget formEnctype (formSettings merchantId organizationId org)
    Nothing ->
      permissionDenied ""

aForm :: OrganizationId -> Organization -> AForm Handler Property
aForm organizationId org = Property
    <$> areq propertyNameField (bfsAutoFocus MsgNameObject) Nothing
    <*> areq hiddenField (bfs MsgNothing) (Just organizationId)
    <*> areq (selectFieldList selectOptionsList) (bfs MsgKind) Nothing
    <*> areq (selectFieldList selectOptionsList) (bfs MsgAvailability) Nothing
    <*> aopt Rentier.FormField.natIntField (bfs MsgQty) (Just (Just 1))
    <*> areq Rentier.FormField.nonNegRatField (bfs $ MsgBasePrice $ organizationBaseCurrencyCode org) Nothing
    <*> areq Rentier.FormField.posTimeField (bfs MsgBaseRentDuration) Nothing
    <*> areq posTimeField (bfs MsgIframeTimeResolution) (Just $ organizationDefaultIframeTimeResolution org)
    <*> areq hiddenField (bfs MsgNothing) (Just True)
    <*> areq hiddenField (bfs MsgNothing) (Just True)
    where
      propertyNameField = checkM (validatePropertyName organizationId) textField

validatePropertyName :: OrganizationId -> Text -> Handler (Either AppMessage Text)
validatePropertyName organizationId name = do
  mm <- runDB $ getBy $ UniqueProperty name organizationId
  return $ case mm of
    Just _  -> Left MsgAlreadyExists
    Nothing -> Right name

formSettings :: MerchantId -> OrganizationId -> Organization -> SimpleFormSettings
formSettings merchantId organizationId org =
  SimpleFormSettings{
    formRoute = PropertyCreateR merchantId organizationId,
    formMsgSubmit = MsgPropertyCreate,
    formPageTitle = MsgPropertyCreateRTitle $ organizationName org,
    formLayout = defaultLayout
  }
