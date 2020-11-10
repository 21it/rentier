{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.OrganizationUpdate where

import qualified Database.Persist      as P
import           Import
import           Rentier.FormField
import           Rentier.HumanReadable
import           Rentier.Property
import           Rentier.Rational
import           Rentier.SimpleForm
import           Rentier.Utils
import           Yesod.Form.Bootstrap3

getOrganizationUpdateR :: MerchantId -> OrganizationId -> Handler Html
getOrganizationUpdateR merchantId organizationId = do
  morg <- runDB $ get organizationId
  case morg of
    Just oldOrg -> do
      (formWidget, formEnctype) <- generateFormPost $
                                   renderBootstrap3 BootstrapBasicForm
                                   (aForm merchantId organizationId oldOrg)
      renderPage merchantId organizationId oldOrg formWidget formEnctype (formSettings merchantId organizationId oldOrg)
    Nothing ->
      permissionDenied ""

postOrganizationUpdateR :: MerchantId -> OrganizationId -> Handler Html
postOrganizationUpdateR merchantId organizationId = do
  morg <- runDB $ get organizationId
  case morg of
    Just oldOrg -> do
      ((formResult, formWidget), formEnctype) <- runFormPost $
                                                 renderBootstrap3 BootstrapBasicForm
                                                 (aForm merchantId organizationId oldOrg)
      case formResult of
          FormSuccess newOrg -> do
            _ <- runDB $ P.update organizationId [
                OrganizationName P.=. organizationName newOrg,
                OrganizationBaseCurrencyCode  P.=. organizationBaseCurrencyCode newOrg,
                OrganizationDefaultMinimalRentDuration  P.=. organizationDefaultMinimalRentDuration newOrg,
                OrganizationDefaultIframeTimeResolution  P.=. organizationDefaultIframeTimeResolution newOrg,
                OrganizationActive P.=. organizationActive newOrg
              ]
            setMessageI $ MsgOrganizationUpdated $ organizationName newOrg
            redirect $ OrganizationListR merchantId
          _ ->
            renderPage merchantId organizationId oldOrg formWidget formEnctype (formSettings merchantId organizationId oldOrg)
    Nothing ->
      permissionDenied ""

aForm :: MerchantId -> OrganizationId -> Organization -> AForm Handler Organization
aForm merchantId organizationId org = Organization
    <$> areq organizationNameField (bfs MsgOrganizationName) (Just $ organizationName org)
    <*> areq hiddenField (bfs MsgNothing) (Just merchantId)
    <*> areq (selectField optionsEnum) (bfs MsgCurrencyCode) (Just $ organizationBaseCurrencyCode org)
    <*> areq posTimeField (bfs MsgDefaultMinimalRentDuration) (Just $ organizationDefaultMinimalRentDuration org)
    <*> areq posTimeField (bfs MsgDefaultIframeTimeResolution) (Just $ organizationDefaultIframeTimeResolution org)
    <*> areq checkBoxField (bfs MsgActive) (Just $ organizationActive org)
    <*> areq hiddenField (bfs MsgNothing) (Just $ organizationExists org)
    where
      organizationNameField = checkM (validateOrganizationName merchantId organizationId) textField

validateOrganizationName :: MerchantId -> OrganizationId -> Text -> Handler (Either AppMessage Text)
validateOrganizationName merchantId organizationId organizationName = do
  morg <- runDB $ getBy $ UniqueOrganization organizationName merchantId
  return $ case morg of
    Just Entity{entityKey = dbOrganizationId} | dbOrganizationId /= organizationId -> Left MsgAlreadyExists
    Just Entity{} -> Right organizationName
    Nothing -> Right organizationName

formSettings :: MerchantId -> OrganizationId -> Organization -> SimpleFormSettings
formSettings merchantId organizationId organization =
  SimpleFormSettings{
    formRoute = OrganizationUpdateR merchantId organizationId,
    formMsgSubmit = MsgOrganizationUpdate,
    formPageTitle = MsgOrganizationUpdateRTitle $ organizationName organization,
    formLayout = defaultLayout
  }

renderPage :: MerchantId -> OrganizationId -> Organization -> Widget -> Enctype -> SimpleFormSettings -> Handler Html
renderPage merchantId organizationId org formWidget formEnctype SimpleFormSettings{formRoute, formMsgSubmit, formPageTitle} = do
  app <- getYesod
  langs <- languages
  organizationScheduleRules <- runDB $ selectList
    [OrganizationScheduleOrganizationId P.==. organizationId]
    [Asc OrganizationScheduleId]
  organizationProperty <- runDB $ selectList
    [PropertyOrganizationId P.==. organizationId, PropertyExists P.==. True]
    [Asc PropertyId]
  -- schedule
  let tableScheduleRulesWidget =
        makeTableWidget table organizationScheduleRules
        where
          hr2Text :: HumanReadable a => a -> Text
          hr2Text = toText app langs
          hr2ShortText :: HumanReadable a => a -> Text
          hr2ShortText = toShortText app langs
          weekDaysWidget weekDaysList =
            [whamlet|
              <span .hidden-xs>
                $forall weekDay <- weekDaysList
                  <span .show>#{hr2Text weekDay}

              <span .hidden-sm.hidden-md.hidden-lg>
                $forall weekDay <- weekDaysList
                  <span .show>#{hr2ShortText weekDay}
            |]
          table =
                widgetCol (hr2Text MsgWeekDay) (weekDaysWidget . organizationScheduleWeekDays . entityVal) <>
                textCol (hr2Text MsgTime) (\x -> hr2Text (organizationScheduleStartsAt $ entityVal x, organizationScheduleEndsAt $ entityVal x)) <>
                textColHiddenXs (hr2Text MsgMinimalRentDuration) (hr2Text . organizationScheduleMinimalRentDuration . entityVal) <>
                textColHiddenXs (hr2Text MsgTimeKind) (hr2Text . organizationScheduleTimeKind . entityVal) <>
                updateDeleteCol (OrganizationScheduleUpdateR merchantId organizationId . entityKey) (OrganizationScheduleDeleteR merchantId organizationId . entityKey)
  -- property
  let tableProperty =
        textCol (hr2Text MsgNameObject) (propertyName . entityVal) <>
        textColHiddenXs (hr2Text MsgQty) (hr2Text . propertyQty . entityVal) <>
        textCol (hr2Text $ MsgBasePrice $ organizationBaseCurrencyCode org) (showRational . propertyBasePrice . entityVal) <>
        textColHiddenXs (hr2Text MsgBaseRentDuration) (hr2Text . propertyBaseRentDuration . entityVal) <>
        textColHiddenXs (hr2Text MsgIframeTimeResolution) (hr2Text . propertyIframeTimeResolution . entityVal) <>
        textCol (hr2Text MsgAvailability) (hr2Text . propertyAvailability . entityVal) <>
        textColHiddenXs (hr2Text MsgActive) (hr2Text . propertyActive . entityVal)
        where
          hr2Text :: HumanReadable a => a -> Text
          hr2Text = toText app langs
  let organizationPrimaryProperty =
        filter (\x -> propertyKind (entityVal x) == Primary) organizationProperty
  let organizationSecondaryProperty =
        filter (\x -> propertyKind (entityVal x) == Secondary) organizationProperty
  let tablePrimaryPropertyWidget =
        makeTableWidget
          (tableProperty <> updateShowCol (PropertyUpdateR merchantId organizationId . entityKey) (IframePropertyScheduleR . entityKey))
          organizationPrimaryProperty
  let tableSecondaryPropertyWidget =
        makeTableWidget
          (tableProperty <> updateCol (PropertyUpdateR merchantId organizationId . entityKey))
          organizationSecondaryProperty
  -- render
  defaultLayout $ do
    setTitleI formPageTitle
    $(widgetFile "organization_update")
  where
    organizationFormWidget = $(widgetFile "simple_form")
