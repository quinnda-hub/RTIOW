module Main where

import           BVH     (buildBVH)
import           Camera  (Camera (..), createCamera)
import           Image   (writeImage)
import           Math    (R)
import           Scenes  (quads)
import           Vec3    (Vec3 (..))

aspectRatio :: R
aspectRatio = 1.0

imageWidth :: Int
imageWidth = 400

camera :: Camera
camera = createCamera aspectRatio imageWidth 100 80 (Vec3 0 0 9) (Vec3 0 0 0) (Vec3 0 1 0) 0.0 1

main :: IO ()
main = do 
    let scene = quads
    writeImage camera (buildBVH scene) 10
