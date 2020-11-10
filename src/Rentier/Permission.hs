module Rentier.Permission where
import           Model
import           Rentier.UserRole

data Permission = ViewProfile
                  | UserRoleIs UserRole
                  | ManageMerchant MerchantId
                  | ManageOrganization MerchantId OrganizationId
                  | ManageOrganizationSchedule OrganizationId OrganizationScheduleId
                  | ManageProperty OrganizationId PropertyId
