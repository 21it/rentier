module Rentier.UserRole where
import           Model
data UserRole = UserRoleMerchant MerchantId | UserRoleEmployee | UserRoleCustomer deriving (Show, Read, Eq)
