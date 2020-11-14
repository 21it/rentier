module Rentier.Property where

import Database.Persist.TH
import Import.External

data PropertyKind
  = Primary
  | Secondary
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "PropertyKind"

data PropertyAvailability
  = InOrganization
  | Anytime
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "PropertyAvailability"
