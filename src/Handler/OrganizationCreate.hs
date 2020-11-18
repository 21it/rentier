{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.OrganizationCreate where

import Import
import Rentier.FormField
import Rentier.Rational ()
import Rentier.SimpleForm
import Rentier.Utils
import Yesod.Form.Bootstrap3

getOrganizationCreateR :: MerchantId -> Handler Html
getOrganizationCreateR merchantId = do
  (_, user) <- requireAuthPair
  (formWidget, formEnctype) <-
    generateFormPost $
      renderBootstrap3
        BootstrapBasicForm
        (aForm merchantId)
  renderSimpleForm formWidget formEnctype (formSettings user merchantId)

postOrganizationCreateR :: MerchantId -> Handler Html
postOrganizationCreateR merchantId = do
  (_, user) <- requireAuthPair
  ((formResult, formWidget), formEnctype) <-
    runFormPost $
      renderBootstrap3
        BootstrapBasicForm
        (aForm merchantId)
  case formResult of
    FormSuccess org -> do
      organizationId <-
        runDB $
          insert400
            Organization
              { organizationName = organizationName org,
                organizationMerchantId = merchantId,
                organizationBaseCurrencyCode = organizationBaseCurrencyCode org,
                organizationDefaultMinimalRentDuration = organizationDefaultMinimalRentDuration org,
                organizationDefaultIframeTimeResolution = organizationDefaultIframeTimeResolution org,
                organizationActive = True,
                organizationExists = True
              }
      setMessageI $ MsgOrganizationCreated $ organizationName org
      redirect $ OrganizationUpdateR merchantId organizationId
    _ ->
      renderSimpleForm formWidget formEnctype (formSettings user merchantId)

aForm :: MerchantId -> AForm Handler Organization
aForm merchantId =
  Organization
    <$> areq organizationNameField (bfsAutoFocus MsgOrganizationName) Nothing
    <*> areq hiddenField (bfs MsgNothing) (Just merchantId)
    <*> areq (selectField optionsEnum) (bfs MsgCurrencyCode) Nothing
    <*> areq posTimeField (bfs MsgDefaultMinimalRentDuration) Nothing
    <*> areq posTimeField (bfs MsgDefaultIframeTimeResolution) Nothing
    <*> areq hiddenField (bfs MsgNothing) (Just True)
    <*> areq hiddenField (bfs MsgNothing) (Just True)
  where
    organizationNameField = checkM (validateOrganizationName merchantId) textField

validateOrganizationName :: MerchantId -> Text -> Handler (Either AppMessage Text)
validateOrganizationName merchantId name = do
  morg <- runDB $ getBy $ UniqueOrganization name merchantId
  return $ case morg of
    Just _ -> Left MsgAlreadyExists
    Nothing -> Right name

formSettings :: User -> MerchantId -> SimpleFormSettings
formSettings user merchantId =
  SimpleFormSettings
    { formRoute = OrganizationCreateR merchantId,
      formMsgSubmit = MsgOrganizationCreate,
      formPageTitle = MsgOrganizationCreateRTitle user,
      formLayout = defaultLayout
    }
