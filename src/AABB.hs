{- |
Module      :  AABB
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module provides the Axis-Aligned Bounding Box (AABB) structure and functions for ray tracing. The AABB is defined by three intervals, one for each axis (x, y, z). 

The key functions provided are:
  
  * `makeAABB`: Constructs an AABB from two 3D points, treating them as the minimum and maximum extrema.
  
  * `hitAABB`: Checks if a ray intersects with the AABB, taking into account the ray's direction and a valid t-interval. It iterates over the three axes, adjusting the ray's t-interval as necessary to determine whether the ray passes through the bounding box.
  
-}

module AABB where

import           Interval (Interval (..), enclosingInterval)
import Vec3 (Vec3(..))
import Ray (Ray(..))

data AABB = AABB { xInterval, yInterval, zInterval :: Interval} 
                 | AABBEmpty 
                 deriving (Show, Eq)

-- Constructs an AABB from two points by treating them as the extrema of the bounding box.
makeAABB :: Vec3 -> Vec3 -> AABB 
makeAABB (Vec3 ax ay az) (Vec3 bx by bz) = AABB x y z 
  where 
    x = if ax <= bx then Interval ax bx else Interval bx ax 
    y = if ay <= by then Interval ay by else Interval by ay 
    z = if az <= bz then Interval az bz else Interval bz az

-- Checks if a ray intersects with the given AABB. 
hitAABB :: AABB -> Ray -> Interval -> Bool 
hitAABB AABBEmpty _ _ = False 
hitAABB (AABB x y z) (Ray origin direction _) rayT = 
    foldr checkAxis True [(x, xComp), (y, yComp), (z, zComp)]
      where 
        checkAxis (interval, compFunc) acc = 
            let invDir  = 1.0 / compFunc direction 
                t0      = (iMin interval - compFunc origin) * invDir 
                t1      = (iMax interval - compFunc origin) * invDir 
                (tMin, tMax) = if t0 < t1 then (t0, t1) else (t1, t0)
                newRayT = Interval (max (iMin rayT) tMin) (min (iMax rayT) tMax)
                in acc && iMax newRayT > iMin newRayT

-- Constructs an AABB from two existing AABBs. 
enclosingAABB :: AABB -> AABB -> AABB 
enclosingAABB (AABB ax ay az) (AABB bx by bz) = 
  AABB (enclosingInterval ax bx) (enclosingInterval ay by) (enclosingInterval az bz) 
enclosingAABB AABBEmpty aabb = aabb 
enclosingAABB aabb AABBEmpty = aabb 
