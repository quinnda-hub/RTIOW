module Texture where

import           Math (R)
import           Vec3 (RGB, Vec3(..))

data Texture = SolidColour RGB 
             | CheckerTexture R Texture Texture 

textureValue :: Texture -> (R, R) -> Vec3 -> RGB 
textureValue (SolidColour colour) _ _ = colour 
textureValue (CheckerTexture scale evenTex oddTex) (u, v) p = 
    let xInt   = floor (scale * xComp p) :: Int
        yInt   = floor (scale * yComp p) :: Int
        zInt   = floor (scale * zComp p) :: Int
        isEven = even (xInt + yInt + zInt)
        in if isEven
            then textureValue evenTex (u, v) p
            else textureValue oddTex (u, v) p

checkerTexture :: R -> Texture -> Texture -> Texture
checkerTexture scale = CheckerTexture (1.0 / scale)

checkerTextureFromColours :: R -> RGB -> RGB -> Texture
checkerTextureFromColours scale c1 c2 = 
    checkerTexture scale (SolidColour c1) (SolidColour c2)
