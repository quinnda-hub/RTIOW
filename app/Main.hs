module Main where

import           BVH     (buildBVH)
import           Camera  (Camera (..), createCamera)
import           Image   (writeImage)
import           Math    (R)
import           Scenes  (cornellBox)
import           Vec3    (Vec3 (..))

aspectRatio :: R
aspectRatio = 1.0

imageWidth :: Int
imageWidth = 600

camera :: Camera
camera = createCamera aspectRatio imageWidth 100 40 (Vec3 278 278 (-800)) (Vec3 278 278 0) (Vec3 0 1 0) 0.0 1 (Vec3 0 0 0)

main :: IO ()
main = do 
    let scene = cornellBox
    writeImage camera (buildBVH scene) 10
