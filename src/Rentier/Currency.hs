{-# LANGUAGE TemplateHaskell #-}
module Rentier.Currency where

import           Database.Persist.TH

data Code = EUR | USD | RUB deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Code"
