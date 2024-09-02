{- | 
Module      :  Utils
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module provides utility functions used across the project. 
Currently, it includes a function `getSecondsNow` that returns 
the current time in seconds with sub-second precision. The function 
leverages Haskell's `Data.Time.Clock` library to retrieve the 
current time and convert it into a format suitable for timing 
calculations.
-}

module Utils where

import Data.Time.Clock (getCurrentTime, utctDayTime)

-- Get the current time in seconds
getSecondsNow :: IO Double
getSecondsNow = do
    currentTime <- getCurrentTime
    let seconds = realToFrac (utctDayTime currentTime)
    return seconds
