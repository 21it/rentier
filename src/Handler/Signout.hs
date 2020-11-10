{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Signout where

import           Import

getSignoutR :: Handler Html
getSignoutR = do
  deleteUserRole
  redirect $ AuthR LogoutR
