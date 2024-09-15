module Main where

import           BVH     (buildBVH)
import           Camera  (Camera (..), createCamera)
import           Image   (loadImageFromFile, writeImage)
import           Math    (R)
import           Scenes  (earth)
import           Texture (Texture (..))
import           Vec3    (Vec3 (..))

aspectRatio :: R
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 3840

camera :: Camera
camera = createCamera aspectRatio imageWidth 100 20 (Vec3 13 2 3) (Vec3 0 0 0) (Vec3 0 1 0) 0.6 10

main :: IO ()
main = do
    tex <- loadImageFromFile "assets/world.jpg"
    case tex of
        Nothing -> putStrLn "Error loading image."
        Just img ->
            let scene = earth (ImageTexture img)
            in writeImage camera (buildBVH scene) 10
