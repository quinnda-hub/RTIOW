module Scenes where

import           Data.Foldable (Foldable (foldl'))
import           Hittable      (Material (..), SomeHittable (SomeHittable))
import           Random        (arbitraryVec3, arbitraryVec3InRange,
                                sampleFraction, sampleFractionInRange)
import           Ray           (Ray (..))
import           Sphere        (Sphere (..))
import           System.Random (StdGen, mkStdGen)
import           Texture       (Texture(..),
                                checkerTextureFromColours)
import           Vec3          (Vec3 (..), magnitude, (^*^), (^-^))

staticBalls :: Int -> [SomeHittable]
staticBalls seed = ground ++ fixed ++ randomObjects
  where
    ground = [SomeHittable $ StaticSphere (Vec3 0 (-1000) 0) 1000 (Lambertian $ SolidColour (Vec3 0.5 0.5 0.5))]

    randomObjects = fst $ foldl' makeObject ([], mkStdGen seed) coordinates
      where
        coordinates = [(a, b) | a <- [-11..11], b <- [-11..11]]

        makeObject :: ([SomeHittable], StdGen) -> (Int, Int) -> ([SomeHittable], StdGen)
        makeObject (objects, g) (a, b) = (maybe objects (: objects) object, g2)
          where
            (Vec3 chooseMat x z, g1) = arbitraryVec3InRange 0 1 g
            center = Vec3 (fromIntegral a + 0.9 * x) 0.2 (fromIntegral b + 0.9 * z)
            (object, g2)
              | magnitude (center ^-^ Vec3 4 0.2 0) > 0.9 = createRandomObject g1
              | otherwise = (Nothing, g1)

            createRandomObject :: StdGen -> (Maybe SomeHittable, StdGen)
            createRandomObject g3 = (Just (SomeHittable obj), g5)
              where
                (p1, g4) = arbitraryVec3 g3
                (p2, g5) = arbitraryVec3 g4
                (d1, _) = sampleFraction g5
                col = p1 ^*^ p2
                mat
                  | chooseMat < 0.8 = Lambertian $ SolidColour col
                  | chooseMat < 0.95 = Metal (Vec3 (0.5 + 0.5 * x1) (0.5 + 0.5 * y1) (0.5 + 0.5 * z1)) (0.5 * d1)
                  | otherwise = Dialectric 1.5
                  where Vec3 x1 y1 z1 = p1
                obj = StaticSphere center 0.2 mat

    fixed = [ SomeHittable $ StaticSphere (Vec3 0 1 0) 1 (Dialectric 1.5)
            , SomeHittable $ StaticSphere (Vec3 (-4) 1 0) 1 (Lambertian $ SolidColour (Vec3 0.4 0.2 0.1))
            , SomeHittable $ StaticSphere (Vec3 4 1 0) 1 (Metal (Vec3 0.7 0.6 0.5) 0)
            ]

bouncingBalls :: Int -> [SomeHittable]
bouncingBalls seed = ground ++ fixed ++ randomObjects
  where
    ground = [SomeHittable $ StaticSphere (Vec3 0 (-1000) 0) 1000 (Lambertian $ SolidColour (Vec3 0.5 0.5 0.5))]

    randomObjects = fst $ foldl' makeObject ([], mkStdGen seed) coordinates
      where
        coordinates = [(a, b) | a <- [-11..11], b <- [-11..11]]

        makeObject :: ([SomeHittable], StdGen) -> (Int, Int) -> ([SomeHittable], StdGen)
        makeObject (objects, g) (a, b) = (maybe objects (: objects) object, g2)
          where
            (Vec3 chooseMat x z, g1) = arbitraryVec3InRange 0 1 g
            center = Vec3 (fromIntegral a + 0.9 * x) 0.2 (fromIntegral b + 0.9 * z)
            (object, g2)
              | magnitude (center ^-^ Vec3 4 0.2 0) > 0.9 = createRandomObject g1
              | otherwise = (Nothing, g1)

            createRandomObject :: StdGen -> (Maybe SomeHittable, StdGen)
            createRandomObject g3 = (Just (SomeHittable obj), g5)
              where
                (p1, g4) = arbitraryVec3 g3
                (p2, g5) = arbitraryVec3 g4
                (d1, g6) = sampleFraction g5
                col = p1 ^*^ p2
                displacement = Vec3 0.0 (fst (sampleFractionInRange g6 0.0 0.5)) 0.0
                mat
                  | chooseMat < 0.8 = Lambertian $ SolidColour col
                  | chooseMat < 0.95 = Metal (Vec3 (0.5 + 0.5 * x1) (0.5 + 0.5 * y1) (0.5 + 0.5 * z1)) (0.5 * d1)
                  | otherwise = Dialectric 1.5
                  where Vec3 x1 y1 z1 = p1
                obj
                  | chooseMat < 0.8 = MovingSphere (Ray center displacement 0.0) 0.2 mat
                  | otherwise       = StaticSphere center 0.2 mat


    fixed = [ SomeHittable $ StaticSphere (Vec3 0 1 0) 1 (Dialectric 1.5)
            , SomeHittable $ StaticSphere (Vec3 (-4) 1 0) 1 (Lambertian $ SolidColour (Vec3 0.4 0.2 0.1))
            , SomeHittable $ StaticSphere (Vec3 4 1 0) 1 (Metal (Vec3 0.7 0.6 0.5) 0)
            ]

bouncingBallsCheckered :: Int -> [SomeHittable]
bouncingBallsCheckered seed = ground ++ fixed ++ randomObjects
  where
    checker = checkerTextureFromColours 0.32 (Vec3 0.0 0.0 0.0) (Vec3 0.9 0.9 0.9)
    ground  = [SomeHittable $ StaticSphere (Vec3 0 (-1000) 0) 1000 (Lambertian checker)]

    randomObjects = fst $ foldl' makeObject ([], mkStdGen seed) coordinates
      where
        coordinates = [(a, b) | a <- [-11..11], b <- [-11..11]]

        makeObject :: ([SomeHittable], StdGen) -> (Int, Int) -> ([SomeHittable], StdGen)
        makeObject (objects, g) (a, b) = (maybe objects (: objects) object, g2)
          where
            (Vec3 chooseMat x z, g1) = arbitraryVec3InRange 0 1 g
            center = Vec3 (fromIntegral a + 0.9 * x) 0.2 (fromIntegral b + 0.9 * z)
            (object, g2)
              | magnitude (center ^-^ Vec3 4 0.2 0) > 0.9 = createRandomObject g1
              | otherwise = (Nothing, g1)

            createRandomObject :: StdGen -> (Maybe SomeHittable, StdGen)
            createRandomObject g3 = (Just (SomeHittable obj), g5)
              where
                (p1, g4) = arbitraryVec3 g3
                (p2, g5) = arbitraryVec3 g4
                (d1, g6) = sampleFraction g5
                col = p1 ^*^ p2
                displacement = Vec3 0.0 (fst (sampleFractionInRange g6 0.0 0.5)) 0.0
                mat
                  | chooseMat < 0.8 = Lambertian $ SolidColour col
                  | chooseMat < 0.95 = Metal (Vec3 (0.5 + 0.5 * x1) (0.5 + 0.5 * y1) (0.5 + 0.5 * z1)) (0.5 * d1)
                  | otherwise = Dialectric 1.5
                  where Vec3 x1 y1 z1 = p1
                obj
                  | chooseMat < 0.8 = MovingSphere (Ray center displacement 0.0) 0.2 mat
                  | otherwise       = StaticSphere center 0.2 mat


    fixed = [ SomeHittable $ StaticSphere (Vec3 0 1 0) 1 (Dialectric 1.5)
            , SomeHittable $ StaticSphere (Vec3 (-4) 1 0) 1 (Lambertian $ SolidColour (Vec3 0.4 0.2 0.1))
            , SomeHittable $ StaticSphere (Vec3 4 1 0) 1 (Metal (Vec3 0.7 0.6 0.5) 0)
            ]

checkeredSpheres :: [SomeHittable]
checkeredSpheres = [sphere1, sphere2]
  where
    checker = checkerTextureFromColours 0.32 (Vec3 0.0 0.0 0.0) (Vec3 0.9 0.9 0.9)
    sphere1 = SomeHittable $ StaticSphere (Vec3 0 (-10) 0) 10 (Lambertian checker)
    sphere2 = SomeHittable $ StaticSphere (Vec3 0 10 0) 10 (Lambertian checker)

earth :: Texture -> [SomeHittable]
earth tex = [globe]
  where globe = SomeHittable $ StaticSphere (Vec3 0 0 0) 2 (Lambertian tex)
