module Main where

import           Camera (Camera (..), createCamera)
import           Image  (writeImage)
import           Math   (R)
import           Scenes (staticBalls, bouncingBalls)
import           Vec3   (Vec3 (..))

aspectRatio :: R
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 400

camera :: Camera
camera = createCamera aspectRatio imageWidth 100 20 (Vec3 13 2 3) (Vec3 0 0 0) (Vec3 0 1 0) 0 10

main :: IO ()
main = writeImage camera (bouncingBalls 42) 10
