{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EmployeeList where

import qualified Database.Persist as P
import Import
import Rentier.HumanReadable
import Rentier.SimpleList
import Rentier.Utils

getEmployeeListR :: MerchantId -> Handler Html
getEmployeeListR merchantId = do
  (_, user) <- requireAuthPair
  list <-
    runDB $
      selectList
        [ EmployeeMerchantId P.==. merchantId,
          EmployeeExists P.==. True
        ]
        [Asc EmployeeId]
  app <- getYesod
  langs <- languages
  let table =
        textCol
          (hr2Text MsgEmployeeIdent)
          (employeeIdent . entityVal)
          <> textCol
            (hr2Text MsgEmployeeInvitationStatus)
            (hr2Text . employeeInvitationStatus . entityVal)
          <> textColHiddenXs
            (hr2Text MsgActive)
            (hr2Text . employeeActive . entityVal)
        where
          -- updateCol (OrganizationUpdateR merchantId . entityKey)

          hr2Text :: HumanReadable a => a -> Text
          hr2Text = toText app langs
  let tableWidget =
        makeTableWidget table list
  renderSimpleList
    list
    tableWidget
    $ simpleListSettings user merchantId

simpleListSettings :: User -> MerchantId -> SimpleListSettings
simpleListSettings user merchantId =
  SimpleListSettings
    { listPageTitle = MsgEmployeeListRTitle user,
      listMsgEmpty = MsgEmployeeEmpty,
      listMsgCreate = MsgEmployeeCreate,
      listCreateRoute = EmployeeCreateR merchantId
    }
