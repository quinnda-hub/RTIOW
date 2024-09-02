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

module Sphere where

import           Hittable (Hit (..), Hittable (..), Material (..))
import           Interval (contains)
import           Math     (R)
import           Ray      (Ray (..), rayAt, setFaceNormal)
import           Vec3     (Vec3 (..), (<.>), (^-^), (^/))


data Sphere = Sphere { sphereCenter   :: !Vec3
                     , sphereRadius   :: !R
                     , sphereMaterial :: !Material
                     }

instance Hittable Sphere where
    {-# INLINE hit #-}
    hit (Sphere center radius material) ray tRange =
        let radius' = max 0 radius
            oc     = center ^-^ rayOrigin ray
            a       = rayDirection ray <.> rayDirection ray
            h       = rayDirection ray <.> oc
            c       = (oc <.> oc) - radius'*radius'
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
                             outwardNormal   = (p ^-^ center) ^/ radius'
                             (normal, front) = setFaceNormal ray outwardNormal
                         in Just (Hit p normal root front material)
