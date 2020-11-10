{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}

module Rentier.FormField where

import           Data.Ratio
import           Import
import qualified Prelude
import           Rentier.Rational
import           RIO.Time

rationalField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Rational
rationalField = Field
  { fieldParse = parseHelper $ \s ->
    case readRational s of
      Just a -> Right a
      _      -> Left $ MsgInvalidNumber s

  , fieldView = \theId name attrs value isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="number" step=any :isReq:required="" value="#{showVal value}">
|]
  , fieldEnctype = UrlEncoded
  }
  where showVal = either id showRational

roundedTimeField :: Field (HandlerFor App) TimeOfDay
roundedTimeField =
  checkM validate timeFieldTypeTime
  where
    validate :: TimeOfDay -> (HandlerFor App) (Either AppMessage TimeOfDay)
    validate value = return $ Right value{todSec = 0}

posTimeField :: Field (HandlerFor App) TimeOfDay
posTimeField =
  checkM validate timeFieldTypeTime
  where
    validate value =
      return $ if value >= TimeOfDay{todHour = 0, todMin = 1, todSec = 0}
        then Right value{todSec = 0}
        else Left MsgNaturalNumIsRequired


posRatField :: Field (HandlerFor App) Rational
posRatField =
  checkM validate rationalField
  where
    validate value =
      return $ if value > 0
        then Right value
        else Left MsgPosNumIsRequired

nonNegRatField :: Field (HandlerFor App) Rational
nonNegRatField =
  checkM validate rationalField
  where
    validate value =
      return $ if value >= 0
        then Right value
        else Left MsgPosNumIsRequired

posIntField :: Field (HandlerFor App) Int
posIntField =
  checkM validate intField
  where
    validate value =
      return $ if value > 0
        then Right value
        else Left MsgPosNumIsRequired

natIntField :: Field (HandlerFor App) Int
natIntField =
  checkM validate intField
  where
    validate value =
      return $ if value >= 1
        then Right value
        else Left MsgNaturalNumIsRequired


multipleSelectField :: (Eq a, Show b, Read b)
                       => [(Text, a, [b])]
                       -> ([(a, b)] -> d)
                       -> (b -> String)
                       -> Field (HandlerFor site) d
multipleSelectField input into formatter =
  Field parse view UrlEncoded
  where
    parse selected _ =
        return $ if length selected /= length input
          then
            Left "input incomplete"
          else
            let
              --
              -- TODO : fix unsafe
              --
              parsed = (Prelude.read . unpack) <$> selected
              rawZip = zip input parsed
              output = (\((_, x, _), y) -> (x, y)) <$> rawZip
            in
              --
              -- TODO : validate rawZip
              --
              Right $ Just $ into output

    view theId name attrs _ isReq =
        --
        -- TODO : fix unsafe
        --
        [whamlet|
          <table .table.table-bordered>
            <tbody>
              $forall (hr, _, range) <- input
                <tr .active>
                  <td>#{hr}
                  <td>
                    <select .form-control.multiple-select-field ##{theId} name=#{name} :isReq:required *{attrs}>
                      $forall x <- range
                        <option value=#{show x}>#{formatter x}
                |]
