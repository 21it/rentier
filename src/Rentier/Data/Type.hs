module Rentier.Data.Type
  ( UserIdent' (..),
    WeekDay (..),
    CookieKind (..),
  )
where

import Database.Persist.TH
import Import.External

newtype UserIdent'
  = UserIdent' Text

data CookieKind
  = UserRole
  deriving (Show, Read, Eq)

data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "WeekDay"
