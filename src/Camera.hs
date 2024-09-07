{- |
Module      :  Camera
Copyright   :  (c) Quinn Anhorn. 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module defines the `Camera` data type and associated functions for
creating a camera in a ray tracing context. The `Camera` module allows
for the generation of rays based on a camera's position, orientation,
and lens characteristics, providing the necessary tools to render a
scene. Functions like `createCamera` and `getRay` are provided to set
up and use the camera, and `rayColour` is used to compute the color of
rays as they interact with objects in the scene.
-}
{-# LANGUAGE BangPatterns #-}

module Camera where

import           BVH           (BVHNode)
import           Hittable      (Hit (..), Hittable (..), Scatterable (scatter))
import           Interval      (interval)
import           Math          (R, degrees2Radians, infinity)
import           Random        (randomInUnitDisk, sampleFraction)
import           Ray           (Ray (..))
import           System.Random (RandomGen)
import           Vec3          (RGB, Vec3 (..), negateV, normalize, zeroV, (*^),
                                (><), (^*), (^*^), (^+^), (^-^))

data Camera = Camera { camLowerLeftCorner :: Vec3
                     , camHorizontal      :: Vec3
                     , camVertical        :: Vec3
                     , camOrigin          :: Vec3
                     , camImageWidth      :: Int
                     , camImageHeight     :: Int
                     , camSamplesPerPixel :: Int
                     , camFOV             :: Int
                     , camLookFrom        :: Vec3
                     , camLookAt          :: Vec3
                     , camVUP             :: Vec3
                     , camLensRadius      :: R
                     , camU, camV, camW   :: Vec3
                     , camFocusDist       :: R
                     } deriving (Show)

createCamera :: R        -- Aspect ratio
             -> Int      -- Image width
             -> Int      -- Samples per pixel
             -> Int      -- Field of view (FOV)
             -> Vec3     -- LookFrom (camera position)
             -> Vec3     -- LookAt (target position)
             -> Vec3     -- Up vector (usually Vec3 0 1 0)
             -> R        -- Aperture (determines the amount of defocus blur)
             -> R        -- Focus distance (distance to the plane in perfect focus)
             -> Camera
createCamera aspectRatio imageWidth samples fov lookFrom lookAt up defocusDist focusDist =
    let theta           = degrees2Radians $ fromIntegral fov
        h               = tan (theta / 2)
        viewportHeight  = 2.0 * h * focusDist
        viewportWidth   = aspectRatio * viewportHeight

        w               = normalize $ lookFrom ^-^ lookAt
        u               = normalize $ up >< w
        v               = w >< u

        origin          = lookFrom
        horizontal      = viewportWidth *^ u
        vertical        = viewportHeight *^ negateV v
        lowerLeftCorner = origin ^-^ (horizontal ^* 0.5) ^-^ (vertical ^* 0.5) ^-^ (focusDist *^ w)

        lensRadius      = focusDist * tan (degrees2Radians (defocusDist / 2.0))
        imageHeight     = round $ fromIntegral imageWidth / aspectRatio
    in Camera lowerLeftCorner horizontal vertical origin imageWidth imageHeight samples fov lookFrom lookAt up lensRadius u v w focusDist

getRay :: RandomGen g
       => Camera
       -> R   -- U coordinate.
       -> R   -- V coordinate.
       -> g
       -> (Ray, g)
getRay camera baseU baseV gen =
    (ray, g')
  where
    -- Generate a random point in the unit disk and scale by lens radius
    (rd', g') = randomInUnitDisk gen
    rd = rd' ^* camLensRadius camera

    -- Calculate the offset for the ray origin based on the lens radius
    offset = (xComp rd *^ camU camera) ^+^ (yComp rd *^ camV camera)

    -- Calculate the new ray origin
    orig = camOrigin camera ^+^ offset

    -- Compute the ray direction
    direction = camLowerLeftCorner camera
                ^+^ (baseU *^ camHorizontal camera)
                ^+^ (baseV *^ camVertical camera)
                ^-^ camOrigin camera
                ^-^ offset

    -- Generate a random ray time.
    time = fst $ sampleFraction g'

    -- Create the ray
    ray = Ray orig direction time

{-# INLINE rayColour #-}
rayColour :: RandomGen g
          => g
          -> Int -- Max recursion depth.
          -> BVHNode
          -> Ray
          -> RGB
rayColour g maxDepth bvh r = loop g maxDepth r (Vec3 1 1 1)
  where
    loop _ 0 _ acc = acc
    loop gen depth ray acc =
        case hit bvh ray (interval 0.001 infinity) of
            Just hitRecord@(Hit _ _ _ _ m _) ->
                case scatter m ray hitRecord gen of
                    (Just (!scatteredRay, !attenuation), gen') ->
                        let newAcc = acc ^*^ attenuation
                        in loop gen' (depth - 1) scatteredRay newAcc
                    (Nothing, _) -> zeroV
            Nothing ->
                let !unitDirection = normalize (rayDirection ray)
                    !t = 0.5 * (yComp unitDirection + 1.0)
                    background = (1.0 - t) *^ Vec3 1 1 1 ^+^ t *^ Vec3 0.5 0.7 1.0
                in acc ^*^ background
