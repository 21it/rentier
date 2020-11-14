module Rentier.Language where

import Data.Text
import Database.Persist.TH
import Import.External
import Yesod.Core.Dispatch

data Code
  = En
  | Ru
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Code"

codeList :: [Code]
codeList =
  [minBound .. maxBound]

instance PathPiece Code where
  fromPathPiece :: Text -> Maybe Code
  fromPathPiece qs = readMaybe $ unpack $ toTitle qs

  toPathPiece :: Code -> Text
  toPathPiece code = toLower $ pack $ show code
