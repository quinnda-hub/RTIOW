{-# LANGUAGE Strict #-}

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

module Random (sampleFraction,
               sampleFractionInRange,
               sampleSquare,
               arbitraryVec3,
               arbitraryVec3InRange,
               generateRandomVecs,
               randomInUnitSphere,
               randomUnitVector,
               randomVec3Hemisphere,
               randomInUnitDisk) where

import           Math          (R)
import           System.Random (Random (randomR), RandomGen, random)
import           Vec3          (Vec3 (..), lengthSquared, magnitude, negateV,
                                normalize, (<.>))

{-# INLINEABLE sampleFraction #-}
sampleFraction :: RandomGen g  => g -> (R, g)
sampleFraction = randomR (0, 1)

sampleFractionInRange :: RandomGen g => g -> R -> R -> (R, g)
sampleFractionInRange gen a b = randomR (min a b, max a b) gen

{-# INLINEABLE sampleSquare #-}
sampleSquare :: RandomGen g => g -> ((R, R), g)
sampleSquare gen =
    let (x, gen')  = randomR (-0.5, 0.5) gen
        (y, gen'') = randomR (-0.5, 0.5) gen'
    in ((x, y), gen'')

{-# INLINEABLE arbitraryVec3 #-}
arbitraryVec3 :: RandomGen g => g -> (Vec3, g)
arbitraryVec3 gen =
    let (x, gen')   = random gen
        (y, gen'')  = random gen'
        (z, gen''') = random gen''
    in (Vec3 x y z, gen''')

{-# INLINEABLE arbitraryVec3InRange #-}
arbitraryVec3InRange :: RandomGen g => R -> R -> g -> (Vec3, g)
arbitraryVec3InRange minVal maxVal gen =
    let (x, gen')  = randomR (minVal, maxVal) gen
        (y, gen'') = randomR (minVal, maxVal) gen'
        (z, gen''') = randomR (minVal, maxVal) gen''
    in (Vec3 x y z, gen''')

{-# INLINABLE generateRandomVecs #-}
generateRandomVecs :: RandomGen g => Int -> (R, R) -> g -> ([Vec3], g)
generateRandomVecs 0 _ gen = ([], gen)
generateRandomVecs n range gen =
    let (vec, gen') = uncurry arbitraryVec3InRange range gen
        (vecs, gen'') = generateRandomVecs (n - 1) range gen'
    in (vec : vecs, gen'')

{-# INLINEABLE randomInUnitSphere #-}
randomInUnitSphere :: RandomGen g => g -> (Vec3, g)
randomInUnitSphere gen =
    let findPoint g' =
            let (p, g'') = arbitraryVec3InRange (-1) 1 g'
            in if lengthSquared p < 1 then (p, g'') else findPoint g''
    in findPoint gen

{-# INLINEABLE randomUnitVector #-}
randomUnitVector :: RandomGen g => g -> (Vec3, g)
randomUnitVector gen =
    let (v, gen') = randomInUnitSphere gen
    in  (normalize v, gen')

{-# INLINEABLE randomVec3Hemisphere #-}
randomVec3Hemisphere :: RandomGen g => g -> Vec3 -> (Vec3, g)
randomVec3Hemisphere gen normal =
    let (onUnitSphere, gen') = randomUnitVector gen
    in
        if onUnitSphere <.> normal > 0.0
            then (onUnitSphere, gen')
            else (negateV onUnitSphere, gen')

{-# INLINEABLE randomInUnitDisk #-}
randomInUnitDisk :: RandomGen g => g -> (Vec3, g)
randomInUnitDisk g =
    let (x, g') = randomR (-1, 1) g
        (y, g'') = randomR (-1, 1) g'
        p = Vec3 x y 0
    in if magnitude p^(2 :: Integer) >= 1 then randomInUnitDisk g'' else (p, g'')
