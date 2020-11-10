{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Language where

import Import
import qualified Rentier.Language

getLanguageR :: Rentier.Language.Code -> Handler ()
getLanguageR language = do
    setLanguage $ toPathPiece language
    setUltDestReferer
    redirectUltDest HomeR
