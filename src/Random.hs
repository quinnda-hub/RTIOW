{- | 
Module      :  Random
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module provides various utilities for generating random values, particularly 
for use in ray tracing. It includes functions for sampling random fractions 
(`sampleFraction`), random vectors (`arbitraryVec3`, `randomUnitVector`), and 
random points within specific geometries (`randomInUnitSphere`, `randomInUnitDisk`). 
These functions leverage Haskell's `RandomGen` to produce random values that 
are crucial for simulating realistic effects such as diffuse reflection and 
soft shadows.
-}

module Random where

import           Math          (R)
import           System.Random (Random (randomR), RandomGen, random)
import           Vec3          (Vec3 (..), lengthSquared, magnitude, negateV,
                                normalize, (<.>))

{-# INLINE sampleFraction #-}
sampleFraction :: RandomGen g  => g -> (R, g)
sampleFraction = randomR (0, 1)

sampleFractionInRange :: RandomGen g => g -> R -> R -> (R, g)
sampleFractionInRange gen a b = randomR (min a b, max a b) gen 

{-# INLINE sampleSquare #-}
sampleSquare :: RandomGen g => g -> ((R, R), g)
sampleSquare gen =
    let (x, gen')  = randomR (-0.5, 0.5) gen
        (y, gen'') = randomR (-0.5, 0.5) gen'
    in ((x, y), gen'')

{-# INLINE arbitraryVec3 #-}
arbitraryVec3 :: RandomGen g => g -> (Vec3, g)
arbitraryVec3 gen =
    let (x, gen')   = random gen
        (y, gen'')  = random gen'
        (z, gen''') = random gen''
    in (Vec3 x y z, gen''')

{-# INLINE arbitraryVec3InRange #-}
arbitraryVec3InRange :: RandomGen g => R -> R -> g -> (Vec3, g)
arbitraryVec3InRange minVal maxVal gen =
    let (x, gen')  = randomR (minVal, maxVal) gen
        (y, gen'') = randomR (minVal, maxVal) gen'
        (z, gen''') = randomR (minVal, maxVal) gen''
    in (Vec3 x y z, gen''')

{-# INLINE randomInUnitSphere #-}
randomInUnitSphere :: RandomGen g => g -> (Vec3, g)
randomInUnitSphere gen =
    let (p, gen') = arbitraryVec3InRange (-1) 1 gen
    in if lengthSquared p < 1 then (p, gen') else randomInUnitSphere gen'

{-# INLINE randomUnitVector #-}
randomUnitVector :: RandomGen g => g -> (Vec3, g)
randomUnitVector gen =
    let (v, gen') = randomInUnitSphere gen
    in  (normalize v, gen')

{-# INLINE randomVec3Hemisphere #-}
randomVec3Hemisphere :: RandomGen g => g -> Vec3 -> (Vec3, g)
randomVec3Hemisphere gen normal =
    let (onUnitSphere, gen') = randomUnitVector gen
    in
        if onUnitSphere <.> normal > 0.0
            then (onUnitSphere, gen')
            else (negateV onUnitSphere, gen')

{-# INLINE randomInUnitDisk #-}
randomInUnitDisk :: RandomGen g => g -> (Vec3, g)
randomInUnitDisk g =
    let (x, g') = randomR (-1, 1) g
        (y, g'') = randomR (-1, 1) g'
        p = Vec3 x y 0
    in if magnitude p^(2 :: Integer) >= 1 then randomInUnitDisk g'' else (p, g'')
