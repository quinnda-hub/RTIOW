{-# LANGUAGE ExistentialQuantification #-}

module Texture where
import           Math (R)
import           Vec3 (RGB, Vec3(..))

class Texture t where
    value :: t -> (R, R) -> Vec3 -> RGB

data SomeTexture = forall t. Texture t => SomeTexture t

newtype SolidColour = SolidColour RGB

data CheckerTexture = CheckerTexture { invScale    :: R
                                     , evenTexture :: SomeTexture
                                     , oddTexture  :: SomeTexture}

instance Texture SomeTexture where
    value (SomeTexture tex) = value tex

instance Texture SolidColour where
    value (SolidColour colour) _ _ = colour

instance Texture CheckerTexture where
    value (CheckerTexture scale oddT evenT) (u, v) p =
        let xInt   = floor (scale * xComp p) :: Int
            yInt   = floor (scale * yComp p) :: Int
            zInt   = floor (scale * zComp p) :: Int
            isEven = even (xInt + yInt + zInt)
        in if isEven
            then value evenT (u, v) p
            else value oddT (u, v) p

checkerTexture :: R -> SomeTexture -> SomeTexture -> CheckerTexture
checkerTexture scale = CheckerTexture (1.0 / scale)

checkerTextureFromColours :: R -> RGB -> RGB -> CheckerTexture 
checkerTextureFromColours scale c1 c2 = 
    checkerTexture scale (SomeTexture $ SolidColour c1) (SomeTexture $ SolidColour c2)
