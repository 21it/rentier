{-# LANGUAGE FlexibleInstances #-}

module Rentier.Data.Type
  ( UserIdent' (..),
    FirstName (..),
    LastName (..),
    WeekDay (..),
    CookieKind (..),
  )
where

import Database.Persist.TH
import Import.External

newtype UserIdent'
  = UserIdent' Text
  deriving
    ( PersistField,
      PersistFieldSql,
      PathPiece,
      Eq,
      Ord,
      Show
    )

newtype FirstName
  = FirstName Text
  deriving (PersistField, PersistFieldSql, Eq, Ord)

newtype LastName
  = LastName Text
  deriving (PersistField, PersistFieldSql, Eq, Ord)

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

--
-- TODO
--
instance ToWidget site UserIdent' where
  toWidget x = toWidget (coerce x :: Text)
