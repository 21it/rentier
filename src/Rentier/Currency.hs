module Rentier.Currency where

import Database.Persist.TH
import Import.External

data Code = EUR | USD | RUB deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Code"
