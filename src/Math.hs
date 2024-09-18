{-# LANGUAGE Strict #-}

{- | 
Module      :  Math
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module provides basic mathematical utilities and type definitions used 
in the ray tracing context. It defines the `R` type as an alias for `Double`, 
and provides constants and functions such as `infinity`, `degrees2Radians` 
for converting degrees to radians, and `linear2Gamma` for converting linear 
color values to gamma-corrected values.
-}

module Math (R, 
             infinity,
             degrees2Radians,
             linear2Gamma) where

type R = Double

infinity :: R
infinity = 1.0 / 0.0

degrees2Radians :: Double -> Double
degrees2Radians degrees = degrees * pi / 180

linear2Gamma :: Double -> Double
linear2Gamma x =
  if x > 0
    then sqrt x else 0
