{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.EmployeeLogin where

import           Import
import           Rentier.UserRole

getEmployeeLoginR :: Handler Html
getEmployeeLoginR = do
  setUserRole UserRoleEmployee
  redirect ProfileR
