{-# LANGUAGE Strict #-}

module RotateY where

import           AABB     (AABB (..))
import           Hittable (Hittable (..), SomeHittable (..))
import           Interval (Interval (..))
import           Math     (R, degrees2Radians, infinity)
import           Vec3     (Vec3 (..))
import Data.List (foldl')

data RotateY = RotateY { object   :: SomeHittable
                       , sinTheta :: Double
                       , cosTheta :: Double
                       , bbox     :: AABB
                       }

rotateY :: SomeHittable -> R -> RotateY
rotateY (SomeHittable obj) angle =
    let radians = degrees2Radians angle
        sinT    = sin radians
        cosT    = cos radians
        bbox'   = boundingBox obj
        aabb    = rotateBoundingBoxY bbox' sinT cosT
    in RotateY (SomeHittable obj) sinT cosT aabb

-- Rotate the AABB  around the y axis and return the new AABB that encloses it. 
rotateBoundingBoxY :: AABB -> R -> R -> AABB
rotateBoundingBoxY (AABB xIn yIn zIn) sinT cosT =
    let

        -- All eight corners of the bounding box.
        corners = [ Vec3 (iMin xIn) (iMin yIn) (iMin zIn)
              , Vec3 (iMax xIn) (iMin yIn) (iMin zIn)
              , Vec3 (iMin xIn) (iMax yIn) (iMin zIn)
              , Vec3 (iMax xIn) (iMax yIn) (iMin zIn)
              , Vec3 (iMin xIn) (iMin yIn) (iMax zIn)
              , Vec3 (iMax xIn) (iMin yIn) (iMax zIn)
              , Vec3 (iMin xIn) (iMax yIn) (iMax zIn)
              , Vec3 (iMax xIn) (iMax yIn) (iMax zIn)
              ]
        
        -- Rotate a point around the y axis. 
        rotatePointY (Vec3 x y z) = Vec3 (cosT*x + sinT*z) y (-(sinT * x) + cosT*z)

        rotatedCorners = map rotatePointY corners 

        -- Find thew new min and points. 
        minRotated = foldl' minVec (Vec3 infinity infinity infinity) rotatedCorners
        maxRotated = foldl' maxVec (Vec3 (-infinity) (-infinity) (-infinity)) rotatedCorners
    in 
        AABB (Interval (xComp minRotated) (xComp maxRotated))
             (Interval (yComp minRotated) (yComp maxRotated))
             (Interval (zComp minRotated) (zComp maxRotated))
rotateBoundingBoxY AABBEmpty _ _ = AABBEmpty

-- Minimum Vec3 between two Vec3s.
minVec :: Vec3 -> Vec3 -> Vec3
minVec (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (min x1 x2) (min y1 y2) (min z1 z2)

-- Maximum Vec3 between two Vec3s.
maxVec :: Vec3 -> Vec3 -> Vec3
maxVec (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (max x1 x2) (max y1 y2) (max z1 z2)
