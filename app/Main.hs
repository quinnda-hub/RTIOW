module Main where

import           BVH     (buildBVH)
import           Camera  (Camera (..), createCamera)
import           Image   (writeImage)
import           Math    (R)
import           Scenes  (perlinSpheres)
import           Vec3    (Vec3 (..))

aspectRatio :: R
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 1200

camera :: Camera
camera = createCamera aspectRatio imageWidth 100 20 (Vec3 13 2 3) (Vec3 0 0 0) (Vec3 0 1 0) 0.6 10

main :: IO ()
main = do 
    let scene = perlinSpheres
    writeImage camera (buildBVH scene) 10
