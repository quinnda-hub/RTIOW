module Main where

import           BVH    (BVHNode, buildBVH)
import           Camera (Camera (..), createCamera)
import           Image  (writeImage)
import           Math   (R)
import           Scenes (staticBalls)
import           Vec3   (Vec3 (..))

aspectRatio :: R
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 400

camera :: Camera
camera = createCamera aspectRatio imageWidth 100 20 (Vec3 13 2 3) (Vec3 0 0 0) (Vec3 0 1 0) 0 10

bvh :: BVHNode
bvh = buildBVH (staticBalls 42)

main :: IO ()
main = writeImage camera bvh 10
