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
               makePerlin,
               turb
               ) where

import           Data.Bits           (Bits (..))
import           Data.Vector.Unboxed (Vector, unsafeIndex)
import qualified Data.Vector.Unboxed as V
import           Math                (R)
import           Random              (generateRandomVecs)
import           System.Random       (Random (..), RandomGen, mkStdGen)
import           Vec3                (Vec3 (..), (<.>), (^*))


data Perlin = Perlin { randVecs :: Vector Vec3
                     , permX    :: Vector Int
                     , permY    :: Vector Int
                     , permZ    :: Vector Int
                     }

pointCount :: Int
pointCount = 256

-- Initializes the random values and permutation vectors for noise generation.
makePerlin :: Int -> Perlin
makePerlin seed =
    let gen = mkStdGen seed
        (randomVals, gen') = generateRandomVecs pointCount (-1, 1) gen
        x = generatePerm gen'
        y = generatePerm gen'
        z = generatePerm gen'
    in Perlin (V.fromList randomVals) x y z

-- Generates a Perlin noise texture, a procedural texture that is repeatable and produces smooth randomness.
-- The noise is generated using a hash of a 3D point (x, y, z) and random permutations.
perlinNoise :: Perlin -> Vec3 -> R
perlinNoise perlin (Vec3 x y z) =
    let u = x - fromIntegral (floor x :: Int)
        v = y - fromIntegral (floor y :: Int )
        w = z - fromIntegral (floor z :: Int)

        i = floor x .&. 255
        j = floor y .&. 255
        k = floor z .&. 255

    -- Collect the corner unit vectors for interpolation.
        c = V.generate 8 $ \idx ->
              let di = idx `shiftR` 2 .&. 1  -- Extract bit 2 (di)
                  dj = idx `shiftR` 1 .&. 1  -- Extract bit 1 (dj)
                  dk = idx .&. 1               -- Extract bit 0 (dk)
              in randVecs perlin `unsafeIndex` ((permX perlin `unsafeIndex` ((i + di) .&. 255))
                             `xor` (permY perlin `unsafeIndex` ((j + dj) .&. 255))
                             `xor` (permZ perlin `unsafeIndex` ((k + dk) .&. 255)))

    in trilinearInterp c (u, v, w)

-- Multiple summed frequencies.
turb :: Perlin -> Vec3 -> Int -> R
turb perlin p depth =
    let loop 0 _ _ accum = abs accum
        loop n tempP weight accum =
            let newAccum = accum + weight * perlinNoise perlin tempP
                newTempP = tempP ^* 2    -- Scale the point for higher frequencies
                newWeight = weight * 0.5 -- Reduce the contribution of each successive octave
            in loop (n - 1) newTempP newWeight newAccum
    in loop depth p 1.0 0.0

-- Trilinear interpolation of a 2x2x2 grid cube of values based on fractional coordinates (u, v, w).
trilinearInterp :: Vector Vec3 -> (R, R, R) -> R
trilinearInterp c (u, v, w) =
    let u' = u*u*(3 - 2*u)
        v' = v*v*(3 - 2*v)
        w' = w*w*(3 - 2*w)

        accum = sum [ let i' = fromIntegral (idx `shiftR` 2 .&. 1)
                          j' = fromIntegral (idx `shiftR` 1 .&. 1)
                          k' = fromIntegral (idx .&. 1)
                          weightV = Vec3 (u-i') (v-j') (w-k')
                      in (i'*u' + (1-i')*(1-u'))
                       * (j'*v' + (1-j')*(1-v'))
                       * (k'*w' + (1-k')*(1-w'))
                       * (unsafeIndex c idx <.> weightV )
                  | idx <- [0..7]]
    in accum

-- Generates a random permutation vector used to scramble the x, y, and z coordinates in the Perlin noise function.
generatePerm :: RandomGen g => g -> Vector Int
generatePerm gen = V.fromList $ take pointCount (randomRs (0, pointCount-1) gen)
