{-# LANGUAGE Strict #-}

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

module AABB (AABB(..),
             makeAABB,
             hitAABB,
             boundingBoxMax,
             boundingBoxMin,
             longestAxis,
             enclosingAABB,) where

import           Interval (Interval (..), enclosingInterval, expand, size)
import           Math     (R)
import           Ray      (Ray (..))
import           Vec3     (Vec3 (..), zeroV)

data AABB = AABB { xInterval, yInterval, zInterval :: Interval }
                 | AABBEmpty
                 deriving (Show, Eq)

-- Constructs an AABB from two points by treating them as the extrema of the bounding box.
makeAABB :: Vec3 -> Vec3 -> AABB
makeAABB (Vec3 ax ay az) (Vec3 bx by bz) = padToMinimum $
  AABB x y z
  where
    x = if ax <= bx then Interval ax bx else Interval bx ax
    y = if ay <= by then Interval ay by else Interval by ay
    z = if az <= bz then Interval az bz else Interval bz az

-- Checks if a ray intersects with the given AABB.
{-# INLINE hitAABB #-}
hitAABB :: AABB -> Ray -> Interval -> Bool
hitAABB AABBEmpty _ _ = False
hitAABB (AABB x y z) (Ray origin direction _) rayT =
    let invDirX = 1.0 / xComp direction
        invDirY = 1.0 / yComp direction
        invDirZ = 1.0 / zComp direction
        tMin = iMin rayT
        tMax = iMax rayT
        {-# INLINE checkAxis #-}
        checkAxis minVal maxVal originComp invDir =
          let t0 = (minVal - originComp) * invDir
              t1 = (maxVal - originComp) * invDir
              (tMin', tMax') = if t0 < t1 then (t0, t1) else (t1, t0)
              tMinNew = max tMin tMin'
              tMaxNew = min tMax tMax'
          in tMaxNew > tMinNew

    in checkAxis (iMin x) (iMax x) (xComp origin) invDirX &&
       checkAxis (iMin y) (iMax y) (yComp origin) invDirY &&
       checkAxis (iMin z) (iMax z) (zComp origin) invDirZ

-- Constructs an AABB from two existing AABBs.
enclosingAABB :: AABB -> AABB -> AABB
enclosingAABB (AABB ax ay az) (AABB bx by bz) =
  AABB (enclosingInterval ax bx) (enclosingInterval ay by) (enclosingInterval az bz)
enclosingAABB AABBEmpty aabb = aabb
enclosingAABB aabb AABBEmpty = aabb

-- Get the minimum point from an AABB.
boundingBoxMin :: AABB -> Vec3
boundingBoxMin (AABB x y z) = Vec3 (iMin x) (iMin y) (iMin z)
boundingBoxMin AABBEmpty    = zeroV

-- Get the max point of an AABB.
boundingBoxMax :: AABB -> Vec3
boundingBoxMax (AABB x y z) = Vec3 (iMax x) (iMax y) (iMax z)
boundingBoxMax AABBEmpty    = zeroV

-- Ensure that no side of the AABB is smaller than some delta, padding if necessary.
padToMinimum :: AABB -> AABB
padToMinimum (AABB ix iy iz) =
  let
    delta = 0.0001
  in
    AABB (if size ix < delta then expand ix delta else ix)
         (if size iy < delta then expand iy delta else iy)
         (if size iz < delta then expand iz delta else iz)
padToMinimum AABBEmpty = AABBEmpty

-- Find the longest axis of an AABB.
longestAxis :: AABB -> (Vec3 -> R)
longestAxis (AABB x y z) =
  let xSize = iMax x - iMin x
      ySize = iMax y - iMin y
      zSize = iMax z - iMin z
  in if xSize > ySize
    then if xSize > zSize then xComp else zComp
    else if ySize > zSize then yComp else zComp
longestAxis AABBEmpty = xComp -- Just use the x axis as a default for an empty AABB.
