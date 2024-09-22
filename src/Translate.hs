{-# LANGUAGE Strict #-}

module Translate (Translate(..), 
                 translate) where

import           AABB     (translateAABB)
import           Hittable (Hit (..), Hittable (..), SomeHittable (..))
import           Ray      (Ray (..))
import           Vec3     (Vec3, (^+^), (^-^))

data Translate = Translate { object :: SomeHittable
                           , offset :: Vec3
                           }

-- Translate an object with some offset.
translate :: SomeHittable -> Vec3 -> Translate
translate = Translate

instance Hittable Translate where
  hit (Translate (SomeHittable obj) off) ray tRange =
    let offsetRay = ray { rayOrigin = rayOrigin ray ^-^ off }
    in case hit obj offsetRay tRange of
      Just hitRecord -> Just hitRecord { hitPoint = hitPoint hitRecord ^+^ off }
      Nothing        -> Nothing
  boundingBox (Translate (SomeHittable obj) off) = 
    translateAABB bbox off 
      where 
        bbox = boundingBox obj 
