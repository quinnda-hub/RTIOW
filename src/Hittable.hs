{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}

{- |
Module      :  Hittable
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module defines the `Hittable` class, the `Hit` data type, and various
material types (`Lambertian`, `Metal`, `Dialectric`) that implement the
`Scatterable` class. The `Hittable` class is used to determine if a ray
intersects an object, while the `Scatterable` class describes how rays
interact with materials upon intersection. The module also provides utility
functions like `hitList` for checking intersections across a collection of
objects and implements the Schlick approximation for reflectivity.
-}

module Hittable where

import           AABB          (AABB)
import           Interval      (Interval (..))
import           Math          (R)
import           Random        (randomInUnitSphere, randomUnitVector,
                                sampleFraction)
import           Ray           (Ray (..))
import           System.Random (RandomGen)
import           Vec3          (RGB, Vec3 (..), nearZero, negateV, normalize,
                                reflect, refract, (<.>), (^*), (^+^))

data Hit = Hit { hitPoint     :: !Vec3     -- The point of intersection.
               , hitNormal    :: !Vec3     -- The normal at the intersection.
               , hitT         :: !R        -- The t parameter where the intersection occurs.
               , hitFrontFace :: !Bool     -- Whether the intersection is on the front face.
               , hitMaterial  :: !Material -- The material of the object that was hit.
               }

class Hittable a where
    hit :: a         -- Some object 'a'.
        -> Ray       -- A ray.
        -> Interval
        -> Maybe Hit
    boundingBox :: a -> AABB

data World = World { getHittables :: [SomeHittable]
                   , getWorldBox  :: AABB
                   }

data SomeHittable where
    SomeHittable :: Hittable a => a -> SomeHittable

class Scatterable a where
    scatter :: RandomGen g => a -> Ray -> Hit -> g -> (Maybe (Ray, RGB), g)

data Material where
    Lambertian :: RGB -> Material
    Metal      :: RGB -> R -> Material
    Dialectric :: R -> Material

instance Scatterable Material where
    scatter (Lambertian albedo) (Ray _ _ time) (Hit p normal _ _ _) g =
        let (sampled, g') = randomUnitVector g
            direction     = sampled ^+^ normal
            direction'    = if nearZero direction then normal else direction
            scattered     = Ray p direction' time
        in (Just (scattered, albedo), g')

    scatter (Metal albedo fuzz) (Ray _ direction time) (Hit p normal _ _ _) g =
        let (sampled, g')   = randomInUnitSphere g
            reflected       = reflect (normalize direction) normal
            scattered       = Ray p (reflected ^+^ sampled ^* min fuzz 1.0) time
        in if normal <.> reflected > 0
            then (Just (scattered, albedo), g')
            else (Nothing, g')

    scatter (Dialectric refIdx) (Ray _ direction time) h g =
        let attenuation   = Vec3 1 1 1
            cosTheta      = min (hitNormal h <.> negateV (normalize direction)) 1.0
            sinTheta      = sqrt (1.0 - cosTheta * cosTheta)
            ratio         = if hitFrontFace h then 1.0 / refIdx else refIdx
            refProb       = schlick cosTheta ratio
            (sampled, g') = sampleFraction g
            direction'    = if ratio * sinTheta > 1 || refProb > sampled
                            then reflect (normalize direction) (hitNormal h)
                            else refract (normalize direction) (hitNormal h) ratio
            scattered     = Ray (hitPoint h) direction' time
        in (Just (scattered, attenuation), g')

schlick :: R -> R -> R
schlick cosine refIdx = r0 + (1-r0) * (1-cosine) ^ (5 :: Integer)
    where
        r0 = ((1-refIdx) / (1+refIdx)) ^ (2 :: Integer)
