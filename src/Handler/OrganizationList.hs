{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.OrganizationList where

import qualified Data.Text
import qualified Database.Persist as P
import Import
import Rentier.HumanReadable
import Rentier.SimpleList
import Rentier.Utils

getOrganizationListR :: MerchantId -> Handler Html
getOrganizationListR merchantId = do
  (_, user) <- requireAuthPair
  list <-
    runDB $
      selectList
        [ OrganizationMerchantId P.==. merchantId,
          OrganizationExists P.==. True
        ]
        [Desc OrganizationId]
  app <- getYesod
  langs <- languages
  let table =
        textCol
          (hr2Text MsgOrganizationName)
          (organizationName . entityVal)
          <> textCol
            (hr2Text MsgCurrencyCode)
            (Data.Text.pack . show . organizationBaseCurrencyCode . entityVal)
          <> textCol
            (hr2Text MsgDefaultMinimalRentDuration)
            (hr2Text . organizationDefaultMinimalRentDuration . entityVal)
          <> textColHiddenXs
            (hr2Text MsgDefaultIframeTimeResolution)
            (hr2Text . organizationDefaultIframeTimeResolution . entityVal)
          <> textColHiddenXs
            (hr2Text MsgActive)
            (hr2Text . organizationActive . entityVal)
          <> updateCol
            (OrganizationUpdateR merchantId . entityKey)
        where
          hr2Text :: HumanReadable a => a -> Text
          hr2Text = toText app langs
  let tableWidget =
        makeTableWidget table list
  renderSimpleList list tableWidget (simpleListSettings user merchantId)

simpleListSettings :: User -> MerchantId -> SimpleListSettings
simpleListSettings user merchantId =
  SimpleListSettings
    { listPageTitle = MsgOrganizationListRTitle user,
      listMsgEmpty = MsgOrganizationEmpty,
      listMsgCreate = MsgOrganizationCreate,
      listCreateRoute = OrganizationCreateR merchantId
    }
