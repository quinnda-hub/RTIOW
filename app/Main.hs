module Main where

import           BVH     (buildBVH)
import           Camera  (Camera (..), createCamera)
import           Image   (writeImage, loadImageFromFile)
import           Math    (R)
import           Scenes  (finalScene)
import           Vec3    (Vec3 (..))
import Texture (Texture(ImageTexture))

aspectRatio :: R
aspectRatio = 1.0

imageWidth :: Int
imageWidth = 100

camera :: Camera
camera = createCamera aspectRatio imageWidth 100 40 (Vec3 478 278 (-600)) (Vec3 278 278 0) (Vec3 0 1 0) 0.0 1 (Vec3 0 0 0)

main :: IO ()
main = do
    earth <- loadImageFromFile "assets/world.jpg" 
    case earth of 
        Nothing -> error "Failure to load earth texture." 
        Just e  -> 
            let tex   = ImageTexture e 
                scene = finalScene 42 tex in 
             writeImage camera (buildBVH scene) 10
