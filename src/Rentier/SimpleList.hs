{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Rentier.SimpleList where

import           Import

data SimpleListSettings = SimpleListSettings {
  listPageTitle   :: AppMessage,
  listMsgEmpty    :: AppMessage,
  listMsgCreate   :: AppMessage,
  listCreateRoute :: Route App
}

renderSimpleList :: [a] -> Widget -> SimpleListSettings -> Handler Html
renderSimpleList list tableWidget SimpleListSettings{listPageTitle, listMsgEmpty, listMsgCreate, listCreateRoute} =
  defaultLayout $ do
    setTitleI listPageTitle
    $(widgetFile "simple_list")
