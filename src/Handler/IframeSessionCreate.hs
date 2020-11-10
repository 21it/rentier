{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.IframeSessionCreate where

import           Data.Coerce
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Import
import           Rentier.FormField
import qualified Rentier.Session
import           Rentier.SimpleForm
import           Rentier.Time
import           Rentier.Utils
import           Yesod.Form.Bootstrap3


data SessionInitForm = SessionInitForm{
  sessionCreateLocation                    :: Maybe Text,
  sessionCreateDay                         :: Maybe Day,
  sessionCreateStartsAt                    :: Maybe TimeOfDay,
  sessionCreateEndsAt                      :: Maybe TimeOfDay,
  sessionCreatePrimaryProperty             :: Maybe Text,
  sessionCreatePropertiesMultipleSelection :: PropertiesMultipleSelection,
  sessionCreateCustomerComment             :: Maybe Textarea
} deriving Show

getIframeSessionCreateR :: PropertyId -> UTC -> UTC -> Handler Html
getIframeSessionCreateR propertyId (UTC startsAt) (UTC endsAt) =
  Rentier.Session.createSessionHandler propertyId startsAt endsAt renderer
  where
    renderer :: Organization -> Property -> [Entity Property] -> Handler Html
    renderer org mainProperty ps = do
      hr2Text <- getMessageRender
      (formWidget, formEnctype) <- generateFormPost $
                                   renderBootstrap3 BootstrapBasicForm
                                   (aForm org mainProperty startsAt endsAt ps hr2Text)
      renderSimpleForm formWidget formEnctype (formSettings propertyId startsAt endsAt)

postIframeSessionCreateR :: PropertyId -> UTC -> UTC -> Handler Html
postIframeSessionCreateR propertyId (UTC startsAt) (UTC endsAt) =
  Rentier.Session.createSessionHandler propertyId startsAt endsAt renderer
  where
    renderer :: Organization -> Property -> [Entity Property] -> Handler Html
    renderer org mainProperty ps = do
      hr2Text <- getMessageRender
      ((formResult, formWidget), formEnctype) <- runFormPost $
                                                 renderBootstrap3 BootstrapBasicForm
                                                 (aForm org mainProperty startsAt endsAt ps hr2Text)
      case formResult of
        FormSuccess formSuccess ->
          redirect $ IframeSessionConfirmR
            propertyId
            (UTC startsAt)
            (UTC endsAt)
            (sessionCreatePropertiesMultipleSelection formSuccess)
            (coerce (fromMaybe (Textarea "") (sessionCreateCustomerComment formSuccess)))
        _ ->
          renderSimpleForm formWidget formEnctype (formSettings propertyId startsAt endsAt)

aForm :: Organization -> Property -> UTCTime -> UTCTime -> [Entity Property] -> (AppMessage -> Text) -> AForm Handler SessionInitForm
aForm org mainProperty startsAt endsAt ps hr2Text =
  SessionInitForm
  <$> aopt textField (bfsDisabled MsgLocation) (Just $ Just $ organizationName org)
  <*> aopt dayField (bfsDisabled MsgDate) (Just $ Just $ localDay startsTime)
  <*> aopt timeField (bfsDisabled MsgStartsAt) (Just $ Just $ localTimeOfDay startsTime)
  <*> aopt timeField (bfsDisabled MsgEndsAt) (Just $ Just $ todFromUTCTime endsAt)
  <*> aopt textField (bfsDisabled MsgPropertyPrimary) (Just $ Just $ propertyName mainProperty)
  <*> areq (multipleSelectField selectable PropertiesMultipleSelection (unpack . hr2Text . MsgPcs)) (bfs MsgPropertySecondary) Nothing
  <*> aopt textareaField (bfs MsgComment) Nothing
  where
    startsTime = utcToLocalTime utc startsAt
    selectable =
      (\Entity{entityKey = key, entityVal = Property{propertyName = name, propertyQty = qty}} ->
        case qty of
          Just q  -> (name, key, [0..q])
          Nothing -> (name, key, [0..10]))
      <$> ps

formSettings :: PropertyId -> UTCTime -> UTCTime -> SimpleFormSettings
formSettings propertyId startsAt endsAt =
  SimpleFormSettings{
    formRoute = IframeSessionCreateR propertyId (UTC startsAt) (UTC endsAt),
    formMsgSubmit = MsgContinue,
    formPageTitle = MsgIframeSessionCreateRBreadcrumb,
    formLayout = noLayout
  }
