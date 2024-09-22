module Main where

import           BVH     (buildBVH)
import           Camera  (Camera (..), createCamera)
import           Image   (writeImage)
import           Math    (R)
import           Scenes  (simpleLight)
import           Vec3    (Vec3 (..))

aspectRatio :: R
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 1200

camera :: Camera
camera = createCamera aspectRatio imageWidth 100 20 (Vec3 23 3 6) (Vec3 0 2 0) (Vec3 0 1 0) 0.0 1 (Vec3 0 0 0)

main :: IO ()
main = do 
    let scene = simpleLight
    writeImage camera (buildBVH scene) 10
