module Rentier.BootstrapColor where

import Import.External

data BootstrapColor
  = Success
  | Info
  | Warning
  | Danger
  deriving (Show, Read, Eq)
