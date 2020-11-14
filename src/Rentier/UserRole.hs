module Rentier.UserRole where

import Import.External
import Model

data UserRole
  = UserRoleMerchant MerchantId
  | UserRoleEmployee
  | UserRoleCustomer
  deriving (Show, Read, Eq)
