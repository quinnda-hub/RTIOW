{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict          #-}

{- |
Module      :  Quad
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module defines the `Quad` data type and its instance of the `Hittable` class.
The `Quad` type represents a quadrilateral in 3D space, defined by a point, two 
spanning vectors, and a normal. The `hit` function provides the logic for detecting 
intersections between a ray and the quad, determining if the intersection point lies 
within the bounds of the quad.
-}

module Quad where

import           AABB     (enclosingAABB, makeAABB)
import           Hittable (Hit (..), Hittable (..), Material)
import           Interval (Interval (..), contains)
import           Math     (R)
import           Ray      (Ray (..), rayAt, setFaceNormal)
import           Vec3     (Vec3 (..), (<.>), (><), (^+^), (^-^))

data Quad = Quad { qPoint    :: Vec3 -- The origin point of the quad (Q). 
                 , qU        :: Vec3 -- Spanning vector U of the quad. 
                 , qV        :: Vec3 -- Spanning vector V of the quad. 
                 , qNormal   :: Vec3 -- Normal vector of the quad.
                 , qD        :: R    -- D value in the plane equation.
                 , qMaterial :: Material -- Material of the quad.
                 }

instance Hittable Quad where
    {-# INLINABLE hit #-}
    hit Quad{..} ray tRange
      | abs denom < 1e-8 = Nothing            -- Ray is parallel to the plane
      | not (contains tRange t) = Nothing     -- t is outside the ray interval.
      | not $ isInterior alpha beta = Nothing -- Intersection point is outside the quad.
      | otherwise = Just Hit { hitPoint     = intersection
                             , hitNormal    = normal
                             , hitT         = t
                             , hitFrontFace = front
                             , hitMaterial  = qMaterial
                             , hitTexCoords = (alpha, beta) }
      where denom  = qNormal <.> rayDirection ray
            t      = (qD - qNormal <.> rayOrigin ray) / denom
            hitVec = intersection ^-^ qPoint
            alpha  = (hitVec >< qV) <.> qNormal
            beta   = (hitVec >< qU) <.> qNormal
            (normal, front) = setFaceNormal ray qNormal
            intersection    = rayAt ray t
    boundingBox Quad{..} = enclosingAABB bboxDiagonal1 bboxDiagonal2
      where
        bboxDiagonal1 = makeAABB qPoint (qPoint ^+^ qU ^+^ qV)
        bboxDiagonal2 = makeAABB (qPoint ^+^ qU) (qPoint ^+^ qV)

{-# INLINABLE isInterior #-}
-- Check if the intersection point is inside the quad.
isInterior :: R -> R -> Bool
isInterior alpha beta =
    let unitInterval = Interval 0 1
    in contains unitInterval alpha && contains unitInterval beta
