{-# LANGUAGE Strict #-}

{- |
Module      :  Sphere
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module defines the `Sphere` data type and its instance of the `Hittable`
class. The `Sphere` type represents a sphere in 3D space, characterized by its
center, radius, and material. The `hit` function provides the logic for detecting
intersections between a ray and a sphere, calculating the intersection point,
the normal at that point, and other relevant details. This is a fundamental
component for ray tracing, enabling the rendering of spherical objects in a scene.
-}

module Sphere (Sphere(..)) where

import           AABB     (makeAABB, enclosingAABB)
import           Hittable (Hit (..), Hittable (..), Material (..))
import           Interval (contains)
import           Math     (R)
import           Ray      (Ray (..), rayAt, setFaceNormal)
import           Vec3     (Vec3 (..), (<.>), (^*), (^+^), (^-^), (^/))


data Sphere = StaticSphere { sphereCenter   :: Vec3
                           , sphereRadius   :: R
                           , sphereMaterial :: Material
                           }
            | MovingSphere { sphereCenterRay :: Ray
                           , sphereRadius    :: R
                           , sphereMaterial  :: Material }

instance Hittable Sphere where
    {-# INLINEABLE hit #-}
    hit sphere ray tRange =
        let radius = max 0 (sphereRadius sphere)
            center  = sphereCenterAt sphere (rayTime ray)
            oc      = center ^-^ rayOrigin ray
            a       = rayDirection ray <.> rayDirection ray
            h       = rayDirection ray <.> oc
            c       = (oc <.> oc) - radius*radius
            discriminant = h*h - a*c
        in if discriminant < 0
            then Nothing
            else let sqrtd = sqrt discriminant
                     -- Find the nearest root that lies in
                     -- the acceptable range.
                     root1 = (h - sqrtd) / a
                     root2 = (h + sqrtd) / a
                     root = if contains tRange root1
                            then root1
                            else root2
                 in if not (contains tRange root)
                    then Nothing
                    else let p               = rayAt ray root
                             outwardNormal   = (p ^-^ center) ^/ radius
                             (normal, front) = setFaceNormal ray outwardNormal
                             texCoords       = sphereUV outwardNormal
                         in Just (Hit p normal root front (sphereMaterial sphere) texCoords)

    boundingBox (StaticSphere center radius _) = createStaticSphereBBox center radius
      where
        createStaticSphereBBox c r =
            let radiusVec = Vec3 r r r
            in makeAABB (c^-^radiusVec) (c^+^radiusVec)
    boundingBox sphere@(MovingSphere _ radius _) = 
        let radiusVec = Vec3 radius radius radius
            center1 = sphereCenterAt sphere 0 
            center2 = sphereCenterAt sphere 1 
            box1 = makeAABB (center1^-^radiusVec) (center1^+^radiusVec)
            box2 = makeAABB (center2^-^radiusVec) (center2^+^radiusVec)
        in enclosingAABB box1 box2

{-# INLINEABLE sphereCenterAt #-}
-- Calculate the center of a moving sphere at a given time.
sphereCenterAt :: Sphere -> R -> Vec3
sphereCenterAt (StaticSphere center _ _) _ = center
sphereCenterAt (MovingSphere centerRay _ _) time =
    let origin    = rayOrigin centerRay
        direction = rayDirection centerRay ^* time
    in origin ^+^ direction

{-#  INLINEABLE sphereUV #-}
sphereUV :: Vec3 -> (R, R) 
sphereUV (Vec3 x y z) = 
    let theta = atan2 z x -- Angle in the xz-plane.
        phi   = asin y    -- Angle from the y-axis.     
        u     = 1-(theta+pi) / (2*pi) -- Normalize to [0, 1]. 
        v     = (phi+pi/2) / pi       -- Normalize to [0, 1]. 
    in (u, v)