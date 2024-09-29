{-# LANGUAGE Strict #-}

{- |
Module      :  Interval
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module defines the `Interval` data type and provides utility functions
for working with intervals. The `Interval` type represents a closed interval
with minimum and maximum bounds, or an empty interval. Functions like
`contains`, `surrounds`, and `clamp` allow for checking whether a value lies
within an interval and constraining a value to the interval. The module also
provides commonly used intervals such as `universe` (spanning all real numbers)
and `defaultInterval`.
-}

module Interval (Interval(..),
                 defaultInterval,
                 interval,
                 universe,
                 size,
                 contains,
                 surrounds,
                 clamp,
                 expand,
                 enclosingInterval) where

import           Math (R, infinity)

data Interval = Interval { iMin :: !R
                         , iMax :: !R }
                         | Empty
                         deriving (Show, Eq)

defaultInterval :: Interval
defaultInterval = Empty

interval :: R
         -> R
         -> Interval
interval a b = Interval (min a b) (max a b)

universe :: Interval
universe = Interval (-infinity) infinity

size :: Interval -> R
size (Interval a b) = b - a
size Empty          = 0

-- Check if the interval contains a value.
contains :: Interval -> R -> Bool
contains (Interval a b) x = a <= x && x <= b
contains Empty _          = False

-- Check if the value is strictly within the interval.
surrounds :: Interval -> R -> Bool
surrounds (Interval a b) x = a < x && x < b
surrounds Empty _          = False

-- Constrains a value to lie within the interval.
clamp :: Interval -> R -> R
clamp (Interval a b) x
    | x < a     = a
    | x > b     = b
    | otherwise = x
clamp Empty x = x

-- Expands the interval by a given value.
expand :: Interval -> R -> Interval
expand (Interval a b) x =
    let padding = x / 2
    in Interval (a-padding) (b+padding)
expand Empty _ = Empty

-- Constructs an interval given two existing Intervals.
enclosingInterval :: Interval -> Interval -> Interval
enclosingInterval (Interval amin amax) (Interval bmin bmax) =
    Interval (min amin bmin) (max amax bmax)
enclosingInterval Empty i = i
enclosingInterval i Empty = i
