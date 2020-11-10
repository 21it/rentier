{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Profile where

import qualified Database.Persist      as P
import           Import
import           Rentier.UserRole
import           Rentier.Utils
import           Yesod.Form.Bootstrap3

getProfileR :: Handler Html
getProfileR = do
    (_, model) <- requireAuthPair
    (formWidget, formEnctype) <- generateFormPost $
                                 renderBootstrap3 BootstrapBasicForm
                                 (aForm model)
    renderPage model formWidget formEnctype

postProfileR :: Handler Html
postProfileR = do
  (modelId, oldModel) <- requireAuthPair
  ((formResult, formWidget), formEnctype) <- runFormPost $
                                             renderBootstrap3 BootstrapBasicForm
                                             (aForm oldModel)
  case formResult of
    FormSuccess newModel -> do
      _ <- runDB $ P.update modelId [
          UserFirstName P.=. userFirstName newModel,
          UserLastName P.=. userLastName newModel
        ]
      setMessageI MsgProfilePageUpdated
      redirect ProfileR
    _ ->
      renderPage oldModel formWidget formEnctype

renderPage :: User -> Widget -> Enctype -> Handler Html
renderPage model formWidget formEnctype = do
  mrole <- lookupUserRole
  let ident = userIdent model
  let formRoute = ProfileR
  let formMsgSubmit = MsgProfilePageUpdate
  let form = $(widgetFile "simple_form")
  defaultLayout $ do
      setTitleI $ MsgProfileRTitle ident
      $(widgetFile "profile")

aForm :: User -> AForm Handler User
aForm x = User
          <$> areq hiddenField (bfs MsgNothing) (Just $ userIdent x)
          <*> aopt textField (bfsAutoFocus MsgFirstName) (Just $ userFirstName x)
          <*> aopt textField (bfs MsgLastName) (Just $ userLastName x)
