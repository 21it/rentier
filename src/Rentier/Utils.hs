{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Rentier.Utils where

import           Colonnade
import           Import
import           Rentier.Colored
import           Text.Blaze.Html5.Attributes
import           Yesod.Colonnade
import           Yesod.Form.Bootstrap3

bfsAutoFocus :: RenderMessage site msg => msg -> FieldSettings site
bfsAutoFocus msg =
    bfsStandard {fsAttrs = ("autofocus", "") : fsAttrs bfsStandard}
    where
      bfsStandard = bfs msg

bfsDisabled :: RenderMessage site msg => msg -> FieldSettings site
bfsDisabled msg =
    bfsStandard {fsAttrs = ("disabled", "") : fsAttrs bfsStandard}
    where
      bfsStandard = bfs msg

makeTableWidget :: (Foldable f, Headedness h) =>
                   Colonnade h a (Cell site)
                   -> f a
                   -> WidgetFor site ()
makeTableWidget =
  encodeCellTable [class_ "table table-striped table-bordered table-condensed text-left align-middle"]

widgetCol :: (Colored b, ToWidget site a) =>
               a
               -> (Entity b -> WidgetFor site ())
               -> Colonnade Headed (Entity b) (Cell site)
widgetCol headerLabel renderer =
  headed (Cell [] (toWidget headerLabel)) (\x -> Cell (cellAttributes x) (renderer x))
  where
    cellAttributes x =
      case (color . entityVal) x of
        Just bsColor -> [class_ $ fromString $ toLower $ show bsColor]
        Nothing      -> []

textCol :: (ToWidget site a2, ToWidget site a1, Colored b) =>
           a1 -> (Entity b -> a2) -> Colonnade Headed (Entity b) (Cell site)
textCol headerLabel renderer =
  headed (Cell [] (toWidget headerLabel)) (\x -> Cell (cellAttributes x) (toWidget $ renderer x))
  where
    cellAttributes x =
      case (color . entityVal) x of
        Just bsColor -> [class_ $ fromString $ toLower $ show bsColor]
        Nothing      -> []

textColHiddenXs :: (ToWidget site a2, ToWidget site a1,
                    Colored b) =>
                   a1 -> (Entity b -> a2) -> Colonnade Headed (Entity b) (Cell site)
textColHiddenXs headerLabel renderer =
  headed (Cell [class_ "hidden-xs"] (toWidget headerLabel)) (\x -> Cell (cellAttributes x) (toWidget $ renderer x))
  where
    cellAttributes x =
      case (color . entityVal) x of
        Just bsColor -> [class_ $ fromString $ toLower $ "hidden-xs " ++ show bsColor]
        Nothing    -> [class_ "hidden-xs"]

updateCol :: Colored b =>
             (Entity b -> Route site) -> Colonnade Headed (Entity b) (Cell site)
updateCol updateResolver =
  headed (textCell "") (\x -> Cell (cellAttributes x) (colWidget x))
  where
    cellAttributes x =
      case (color . entityVal) x of
        Just bsColor -> [class_ $ fromString $ toLower $ show bsColor]
        Nothing      -> []
    colWidget x =
      [whamlet|
        <div .show>
          <a .btn.btn-warning.btn-sm href=@{updateResolver x}>
            <span .glyphicon.glyphicon-pencil aria-hidden="true">
      |]

updateShowCol :: Colored b =>
                 (Entity b -> Route site)
                 -> (Entity b -> Route site)
                 -> Colonnade Headed (Entity b) (Cell site)
updateShowCol updateResolver showResolver =
  headed (textCell "") (\x -> Cell (cellAttributes x) (colWidget x))
  where
    cellAttributes x =
      case (color . entityVal) x of
        Just bsColor -> [class_ $ fromString $ toLower $ show bsColor]
        Nothing      -> []
    colWidget x =
      [whamlet|
        <div .show>
          <a .btn.btn-warning.btn-sm href=@{updateResolver x}>
            <span .glyphicon.glyphicon-pencil aria-hidden="true">
        <div .show.small-margin-top>
          <a .btn.btn-info.btn-sm href=@{showResolver x}>
            <span .glyphicon.glyphicon-eye-open aria-hidden="true">
      |]

updateDeleteCol :: Colored b =>
                   (Entity b -> Route site)
                   -> (Entity b -> Route site)
                   -> Colonnade Headed (Entity b) (Cell site)
updateDeleteCol updateResolver deleteResolver =
  headed (textCell "") (\x -> Cell (cellAttributes x) (colWidget x))
  where
    cellAttributes x =
      case (color . entityVal) x of
        Just bsColor -> [class_ $ fromString $ toLower $ show bsColor]
        Nothing      -> []
    colWidget x =
      [whamlet|
        <div .show>
          <a .btn.btn-warning.btn-sm href=@{updateResolver x}>
            <span .glyphicon.glyphicon-pencil aria-hidden="true">
        <div .show.small-margin-top>
          <a .btn.btn-danger.btn-sm href=@{deleteResolver x}>
            <span .glyphicon.glyphicon-trash aria-hidden="true">
      |]
