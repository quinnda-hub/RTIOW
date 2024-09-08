{- |
Module      :  Texture
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module defines the `Texture` data type and provides a way to handle 
different types of textures in a ray tracing context. The `Texture` type
includes solid color textures and checkerboard patterns, which can be 
sampled at given points in space to determine their color. The `textureValue` 
function is used to evaluate the color of a texture based on coordinates 
and the position of a point in space. The module also provides utility 
functions for creating checkerboard textures from solid colors and for 
scaling textures.
-}

module Texture where

import           Math (R)
import           Vec3 (RGB, Vec3(..))

data Texture = SolidColour RGB 
             | CheckerTexture R Texture Texture 

textureValue :: Texture -> (R, R) -> Vec3 -> RGB 
textureValue (SolidColour colour) _ _ = colour 
textureValue (CheckerTexture scale evenTex oddTex) (u, v) p = 
    let xInt   = floor (scale * xComp p) :: Int
        yInt   = floor (scale * yComp p) :: Int
        zInt   = floor (scale * zComp p) :: Int
        isEven = even (xInt + yInt + zInt)
        in if isEven
            then textureValue evenTex (u, v) p
            else textureValue oddTex (u, v) p

checkerTexture :: R -> Texture -> Texture -> Texture
checkerTexture scale = CheckerTexture (1.0 / scale)

checkerTextureFromColours :: R -> RGB -> RGB -> Texture
checkerTextureFromColours scale c1 c2 = 
    checkerTexture scale (SolidColour c1) (SolidColour c2)
