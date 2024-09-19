{-# LANGUAGE Strict #-}

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

module Texture (Texture(..),
               textureValue,
               checkerTextureFromColours) where

import           Codec.Picture       (Image (..), Pixel (pixelAt))
import qualified Codec.Picture.Types as JP
import           Interval            (Interval (..), clamp)
import           Math                (R)
import           Vec3                (RGB, Vec3 (..), (^*))
import Perlin (Perlin, perlinNoise)

data Texture = SolidColour RGB
             | CheckerTexture R Texture Texture
             | ImageTexture (JP.Image JP.PixelRGB8)
             | NoiseTexture Perlin R

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
textureValue (ImageTexture img) (u, v) _ =
    let u' = clamp (Interval 0 1) u
        v' = 1 - clamp (Interval 0 1) v
        i  = floor (u' * fromIntegral (imageWidth img))
        j  = floor (v' * fromIntegral (imageHeight img))
    in pixelToRGB $ pixelAt img (clampCoord i (imageWidth img)) (clampCoord j (imageHeight img))
textureValue (NoiseTexture perlin scale) _ p =
     Vec3 1 1 1 ^* (0.5 * (1 + perlinNoise perlin (p ^* scale)))

checkerTexture :: R -> Texture -> Texture -> Texture
checkerTexture scale = CheckerTexture (1.0 / scale)

checkerTextureFromColours :: R -> RGB -> RGB -> Texture
checkerTextureFromColours scale c1 c2 =
    checkerTexture scale (SolidColour c1) (SolidColour c2)

pixelToRGB :: JP.PixelRGB8 -> RGB
pixelToRGB (JP.PixelRGB8 r g b) = Vec3 (toRange r) (toRange g) (toRange b)
    where
        toRange c = fromIntegral c / 255.0

-- Clamp coordinates so they don't go out of bounds
clampCoord :: Int -> Int -> Int
clampCoord x maxVal
    | x < 0     = 0
    | x >= maxVal = maxVal - 1
    | otherwise = x
