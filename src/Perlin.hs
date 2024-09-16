{-# LANGUAGE BangPatterns #-}

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


module Perlin where

import           Data.Bits     (Bits (..))
import           Data.Vector   (Vector)
import qualified Data.Vector   as V
import           Math          (R)
import           System.Random (Random (..), RandomGen, mkStdGen)

data Perlin = Perlin { randFloats :: !(Vector R)
                     , permX      :: !(Vector Int)
                     , permY      :: !(Vector Int)
                     , permZ      :: !(Vector Int)
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
perlinNoise :: Perlin -> (R, R, R) -> R
perlinNoise perlin (x, y, z) =
    let !i = (truncate (4*x) :: Int) .&. 255
        !j = (truncate (4*y) :: Int) .&. 255
        !k = (truncate (4*z) :: Int) .&. 255
    in randFloats perlin V.! ((permX perlin V.! i) `xor` (permY perlin V.! j) `xor` (permZ perlin V.! k))

-- Generates a random permutation vector used to scramble the x, y, and z coordinates in the Perlin noise function.
generatePerm :: RandomGen g => g -> Vector Int
generatePerm gen = V.fromList $ take pointCount (randomRs (0, pointCount-1) gen)
