module Rentier.Enum where

next :: (Eq a, Enum a, Bounded a) => a -> a
next e | e == maxBound = minBound
       | otherwise = succ e
