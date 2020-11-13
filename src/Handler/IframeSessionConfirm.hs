{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.IframeSessionConfirm where

import qualified Data.List
import qualified Data.Map.Strict as Map
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Import
import Rentier.FormField
import qualified Rentier.Session
import Rentier.SimpleForm
import Rentier.Time
import Rentier.Utils
import Yesod.Form.Bootstrap3

data SessionConfirmForm
  = SessionConfirmForm
      { sessionConfirmLocation :: Maybe Text,
        sessionConfirmDay :: Maybe Day,
        sessionConfirmStartsAt :: Maybe TimeOfDay,
        sessionConfirmEndsAt :: Maybe TimeOfDay,
        sessionConfirmProperties :: Maybe PropertiesMultipleSelection,
        sessionConfirmCustomerComment :: Maybe Textarea
      }
  deriving (Show)

getIframeSessionConfirmR ::
  PropertyId ->
  UTC ->
  UTC ->
  PropertiesMultipleSelection ->
  Text ->
  Handler Html
getIframeSessionConfirmR
  propertyId
  (UTC startsAt)
  (UTC endsAt)
  (PropertiesMultipleSelection additionalProperties)
  customerComment =
    Rentier.Session.createSessionHandler
      propertyId
      startsAt
      endsAt
      renderer
    where
      renderer ::
        Organization ->
        Property ->
        [Entity Property] ->
        Handler Html
      renderer org mainProperty ps = do
        hr2Text <- getMessageRender
        (formWidget, formEnctype) <-
          generateFormPost $
            renderBootstrap3
              BootstrapBasicForm
              ( aForm
                  org
                  propertyId
                  mainProperty
                  startsAt
                  endsAt
                  ps
                  additionalProperties
                  customerComment
                  hr2Text
              )
        renderSimpleForm formWidget formEnctype formSettings

aForm ::
  Organization ->
  PropertyId ->
  Property ->
  UTCTime ->
  UTCTime ->
  [Entity Property] ->
  [(PropertyId, Int)] ->
  Text ->
  (AppMessage -> Text) ->
  AForm Handler SessionConfirmForm
aForm
  org
  propertyId
  mainProperty
  startsAt
  endsAt
  ps
  additionalProperties
  customerComment
  hr2Text =
    SessionConfirmForm
      <$> aopt
        textField
        (bfsDisabled MsgLocation)
        (Just $ Just $ organizationName org)
      <*> aopt
        dayField
        (bfsDisabled MsgDate)
        (Just $ Just $ localDay startsTime)
      <*> aopt
        timeField
        (bfsDisabled MsgStartsAt)
        (Just $ Just $ localTimeOfDay startsTime)
      <*> aopt
        timeField
        (bfsDisabled MsgEndsAt)
        (Just $ Just $ todFromUTCTime endsAt)
      <*> aopt
        (multipleSelectField selectable PropertiesMultipleSelection show)
        (bfsDisabled MsgProperty)
        (Just $ Just $ PropertiesMultipleSelection allProperties)
      <*> aopt
        textareaField
        (bfsDisabled MsgComment)
        (Just $ Just $ Textarea customerComment)
    where
      allProperties = (propertyId, 1) : additionalProperties
      startsTime = utcToLocalTime utc startsAt
      selectedProps =
        Data.List.foldl
          (\acc (k, v) -> Map.insert k v acc)
          Map.empty
          allProperties
      labelFormatter name qty =
        pack $
          "[" <> unpack (hr2Text $ MsgPcs qty) <> "] " <> unpack name
      selectable =
        (Entity {entityKey = propertyId, entityVal = mainProperty} : ps)
          >>= ( \(Entity k v) ->
                  case Map.lookup k selectedProps of
                    Just 0 ->
                      []
                    Just qty ->
                      [(labelFormatter (propertyName v) qty, k, [qty])]
                    Nothing ->
                      []
              )

formSettings :: SimpleFormSettings
formSettings =
  SimpleFormSettings
    { formRoute = HomeR,
      formMsgSubmit = MsgContinue,
      formPageTitle = MsgIframeSessionConfirmRBreadcrumb,
      formLayout = noLayout
    }
