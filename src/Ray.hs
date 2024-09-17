{- | 
Module      :  Ray
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module defines the `Ray` data type, which represents a ray in 3D space, 
along with various utility functions for working with rays. The `Ray` type 
includes fields for the ray's origin, direction, and time. Functions provided 
include `initRay` for initializing a ray, `rayAt` for computing the position 
along a ray at a given parameter, and `setFaceNormal` for determining and 
setting the normal direction relative to the ray. These functions are essential 
for ray tracing calculations.
-}

module Ray (Ray(..),
            rayAt,
            setFaceNormal) where

import           Math (R)
import           Vec3 (Vec3 (..), negateV, (*^), (<.>), (^+^))

data Ray = Ray { rayOrigin    :: !Vec3
               , rayDirection :: !Vec3
               , rayTime      :: !R
               } deriving (Show, Eq)

-- Computes the position along a ray at a given parameter t.
rayAt :: Ray -> R -> Vec3
rayAt (Ray origin direction _) t = origin ^+^ (t *^ direction)

setFaceNormal :: Ray -> Vec3 -> (Vec3, Bool)
setFaceNormal ray outwardNormal =
    let front  = (rayDirection ray <.> outwardNormal) < 0
        normal = if front then outwardNormal else negateV outwardNormal
    in (normal, front)
