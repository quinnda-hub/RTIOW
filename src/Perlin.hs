{-# LANGUAGE Strict #-}

{- |
Module      :  Perlin
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module provides functionality for generating Perlin noise, a type of procedural
noise commonly used in computer graphics for textures, terrain, and smooth random
variations. The core features include generating random values and scrambling 3D
points using permutation vectors to produce repeatable noise patterns.

-}


module Perlin (Perlin(..),
               perlinNoise,
               makePerlin
               ) where

import           Data.Bits     (Bits (..))
import           Data.Vector.Unboxed   (Vector)
import qualified Data.Vector.Unboxed   as V
import           Math          (R)
import           System.Random (Random (..), RandomGen, mkStdGen)
import           Vec3          (Vec3 (..))

data Perlin = Perlin { randFloats :: Vector R
                     , permX      :: Vector Int
                     , permY      :: Vector Int
                     , permZ      :: Vector Int
                     }

pointCount :: Int
pointCount = 256

-- Initializes the random values and permutation vectors for noise generation.
makePerlin :: Int -> Perlin
makePerlin seed =
    let gen = mkStdGen seed
        randomVals = V.fromList (take pointCount (randomRs (0.0, 1.0) gen))  -- Use a vector
        x = generatePerm gen
        y = generatePerm gen
        z = generatePerm gen
    in Perlin randomVals x y z

-- Generates a Perlin noise texture, a procedural texture that is repeatable and produces smooth randomness.
-- The noise is generated using a hash of a 3D point (x, y, z) and random permutations.
perlinNoise :: Perlin -> Vec3 -> R
perlinNoise perlin (Vec3 x y z) =
    -- Precompute the integer and fractional parts of the coordinates
    let xFloor = floor x
        yFloor = floor y
        zFloor = floor z
        i = xFloor .&. 255
        j = yFloor .&. 255
        k = zFloor .&. 255
        u = x - fromIntegral xFloor
        v = y - fromIntegral yFloor
        w = z - fromIntegral zFloor
    -- Collect the corner values from the grid into a flat array
        c = V.generate 8 $ \idx ->
              let di = idx `shiftR` 2 .&. 1  -- Extract bit 2 (di)
                  dj = idx `shiftR` 1 .&. 1  -- Extract bit 1 (dj)
                  dk = idx .&. 1               -- Extract bit 0 (dk)
              in randFloats perlin V.! ((permX perlin V.! ((i + di) .&. 255))
                             `xor` (permY perlin V.! ((j + dj) .&. 255))
                             `xor` (permZ perlin V.! ((k + dk) .&. 255)))
    in trilinearInterp c (u, v, w)

-- Trilinear interpolation of a 2x2x2 grid cube of values based on fractional coordinates (u, v, w).
trilinearInterp :: Vector R -> (R, R, R) -> R
trilinearInterp c (u, v, w) =
    let accum = sum [ let i' = fromIntegral (idx `shiftR` 2 .&. 1)
                          j' = fromIntegral (idx `shiftR` 1 .&. 1)
                          k' = fromIntegral (idx .&. 1)
                      in (i' * u + (1 - i') * (1 - u))
                       * (j' * v + (1 - j') * (1 - v))
                       * (k' * w + (1 - k') * (1 - w))
                       * (c V.! idx)
                  | idx <- [0..7]]
    in accum

-- Generates a random permutation vector used to scramble the x, y, and z coordinates in the Perlin noise function.
generatePerm :: RandomGen g => g -> Vector Int
generatePerm gen = V.fromList $ take pointCount (randomRs (0, pointCount-1) gen)
