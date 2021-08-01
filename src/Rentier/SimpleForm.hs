{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Rentier.SimpleForm where

import Import

data SimpleFormSettings
  = SimpleFormSettings
      { formRoute :: Route App,
        formMsgSubmit :: AppMessage,
        formPageTitle :: AppMessage,
        formLayout :: Widget -> Handler Html
      }

renderSimpleForm :: Widget -> Enctype -> SimpleFormSettings -> Handler Html
renderSimpleForm formWidget formEnctype SimpleFormSettings {formRoute, formMsgSubmit, formPageTitle, formLayout} =
  formLayout $ do
    setTitleI formPageTitle
    $(widgetFile "simple_form")
