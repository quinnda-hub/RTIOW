{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 
Module      :  Vec3
Copyright   :  (c) Scott N. Walck 2012-2019, (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>, qda869@usas.ca
Stability   :  experimental

This module is based on Scott N. Walck's `Physics.Learn.SimpleVec` and 
provides basic operations on the vector type `Vec`, such as vector addition, 
subtraction, scalar multiplication, and more. It retains the simplicity of 
concrete types without the need for type classes, making it easier to use 
and reason about vector operations, especially for those learning Haskell. 

Additions to the original module include the functions `normalize`, `reflect`, 
and `refract`, which provide additional vector operations commonly used in 
ray tracing and other applications.
-}

module Vec3 (Vec3(..),
             RGB,
             iHat,
             jHat,
             kHat,
             zeroV,
             (^+^),
             (^-^),
             (^*^),
             (><),
             (<.>),
             (^*),
             (*^),
             (^/),
             sumV,
             negateV,
             magnitude,
             normalize,
             lengthSquared,
             nearZero,
             reflect,
             refract
             ) where

import           Control.DeepSeq (NFData, rnf)
import           Data.List       (intercalate)
import           Math            (R)
import Data.Vector.Unboxed.Deriving (derivingUnbox)

data Vec3 = Vec3 { xComp :: R
                 , yComp :: R
                 , zComp :: R
                 } deriving (Eq)

type RGB = Vec3

instance Show Vec3 where
  show (Vec3 x y z) = "(" ++ intercalate ", " [show x, show y, show z] ++  ")"

instance NFData Vec3 where
    rnf (Vec3 ax ay az) = rnf ax `seq` rnf ay `seq` rnf az

derivingUnbox "Vec3"
  [t| Vec3 -> (Double, Double, Double) |]
  [| \(Vec3 x y z) -> (x, y, z)|]
  [|\(x, y, z) -> Vec3 x y z|]  

infixl 7 ><
infixl 6 ^+^
infixl 6 ^-^
infixl 6 ^*^
infixl 7 *^
infixl 7 ^*
infixl 7 ^/
infixl 7 <.>  

-- Unit vectors.
iHat :: Vec3
iHat = Vec3 1 0 0

jHat :: Vec3
jHat = Vec3 0 1 0

kHat :: Vec3
kHat = Vec3 0 0 1

-- Zero vector.
zeroV :: Vec3
zeroV = Vec3 0 0 0

-- Addition.
(^+^) :: Vec3 -> Vec3 -> Vec3
Vec3 ax ay az ^+^ Vec3 bx by bz =
    Vec3 (ax+bx) (ay+by) (az+bz)

-- Subtraction.
(^-^) :: Vec3 -> Vec3 -> Vec3
Vec3 ax ay az ^-^ Vec3 bx by bz =
    Vec3 (ax-bx) (ay-by) (az-bz)

-- Multiplication.
(^*^) :: Vec3 -> Vec3 -> Vec3
Vec3 ax ay az ^*^ Vec3 bx by bz =
    Vec3 (ax*bx) (ay*by) (az*bz)

-- Cross product.
(><) :: Vec3 -> Vec3 -> Vec3
Vec3 ax ay az >< Vec3 bx by bz =
    Vec3 (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

-- Dot product.
(<.>) :: Vec3 -> Vec3 -> R
Vec3 ax ay az <.> Vec3 bx by bz =
    ax*bx + ay*by + az*bz

-- Scalar multiplication where scalar is on the right.
(^*) :: Vec3 -> R -> Vec3
Vec3 x y z ^* c = Vec3 (x*c) (y*c) (z*c)

-- Scalar multiplication where scalar is on the left.
(*^) :: R -> Vec3 -> Vec3
c *^ Vec3 x y z = Vec3 (x*c) (y*c) (z*c)

-- Scalar division.
(^/) :: Vec3 -> R -> Vec3
Vec3 x y z ^/ c = Vec3 (x/c) (y/c) (z/c)

-- Sum of a list of vectors.
sumV :: [Vec3] -> Vec3
sumV = foldr (^+^) zeroV

-- Additive inverse of a vector.
negateV :: Vec3 -> Vec3
negateV (Vec3 ax ay az) = Vec3 (-ax) (-ay) (-az)

-- Magnitude (length) of a vector.
magnitude :: Vec3 -> R
magnitude v = sqrt (v <.> v)

-- Normalize a vector to unit lenght. 
normalize :: Vec3 -> Vec3
normalize v = v ^/ magnitude v

-- Calculate the squared length (magnitude) of a vector.
lengthSquared :: Vec3 -> R
lengthSquared (Vec3 ax ay az)  =
  ax*ax + ay*ay + az*az

-- ReturnsTrue if the vector is very close to 0. 
nearZero :: Vec3 -> Bool
nearZero (Vec3 ax ay az) =
    let s = 1e-8
    in abs ax < s && abs ay < s && abs az < s

-- Reflect a vector around a normal.
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n =
    v ^-^ 2*(v<.>n)*^n

-- Refract a vector through a surface with a given normal `n`and a refraction index `ratio`.
refract ::  Vec3 -> Vec3 -> R -> Vec3
refract unitRay normal ratio = rOutPerp ^+^ rOutPerp
    where
        cosTheta     = negateV unitRay <.> normal
        rOutParallel = (unitRay ^+^ normal ^* cosTheta) ^* ratio
        rOutPerp     = negateV $ normal ^* sqrt (1.0 - lengthSquared rOutParallel)
