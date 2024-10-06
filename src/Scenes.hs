module Scenes (staticBalls,
               bouncingBalls,
               bouncingBallsCheckered,
               checkeredSpheres,
               earth,
               perlinSpheres,
               quads,
               simpleLight,
               cornellBox,
               finalScene) where

import           Data.Foldable (Foldable (foldl'))
import           Hittable      (Material (..), SomeHittable (SomeHittable))
import           Perlin        (makePerlin)
import           Quad          (Quad, makeBox, makeQuad, rotateQuad)
import           Random        (arbitraryVec3, arbitraryVec3InRange,
                                sampleFraction, sampleFractionInRange)
import           Ray           (Ray (..))
import           Sphere        (Sphere (..))
import           System.Random (StdGen, mkStdGen)
import           Texture       (Texture (..), checkerTextureFromColours)
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

perlinSpheres :: [SomeHittable]
perlinSpheres =
  let pertext = makePerlin 42
      ground  = SomeHittable $ StaticSphere (Vec3 0 (-1000) 0) 1000 (Lambertian $ NoiseTexture pertext 4)
      sphere  = SomeHittable $ StaticSphere (Vec3 0 2 0) 2 (Lambertian $ NoiseTexture pertext 4)
  in [ground, sphere]

quads :: [SomeHittable]
quads = [left, back, right, upper, lower]
  where
    -- Materials.
    leftRed   = Lambertian $ SolidColour $ Vec3 1.0 0.2 0.2
    backGreen = Lambertian $ SolidColour $ Vec3 0.2 1.0 0.2
    rightBlue = Lambertian $ SolidColour $ Vec3 0.2 0.2 1.0
    uprOrange = Lambertian $ SolidColour $ Vec3 1.0 0.5 0.0
    lwrTeal   = Lambertian $ SolidColour $ Vec3 0.2 0.8 0.8

    -- Quads.
    left  = SomeHittable $ makeQuad (Vec3 (-3.0) (-2.0) 5.0) (Vec3 0.0 0.0 (-4.0)) (Vec3 0.0 4.0 0.0) leftRed
    back  = SomeHittable $ makeQuad (Vec3 (-2.0) (-2.0) 0.0) (Vec3 4.0 0.0 0.0) (Vec3 0.0 4.0 0.0) backGreen
    right = SomeHittable $ makeQuad (Vec3 3.0 (-2.0) 1.0) (Vec3 0.0 0.0 4.0) (Vec3 0.0 4.0 0.0) rightBlue
    upper = SomeHittable $ makeQuad (Vec3 (-2.0) 3.0 1.0) (Vec3 4.0 0.0 0.0) (Vec3 0.0 0.0 4.0) uprOrange
    lower = SomeHittable $ makeQuad (Vec3 (-2.0) (-3.0) 5.0) (Vec3 4.0 0.0 0.0) (Vec3 0.0 0.0 (-4.0)) lwrTeal

simpleLight :: [SomeHittable]
simpleLight = [ground, sphere, light]
  where
    perlinTex = makePerlin 42
    lightTex  = DiffuseLight $ SolidColour $ Vec3 4 4 4
    ground    = SomeHittable $ StaticSphere (Vec3 0 (-1000) 0) 1000 (Lambertian $ NoiseTexture perlinTex 4)
    sphere    = SomeHittable $ StaticSphere (Vec3 0 2 0) 2 (Lambertian $ NoiseTexture perlinTex 4)
    light     = SomeHittable $ makeQuad (Vec3 3.0 1.0 (-2)) (Vec3 2 0 0) (Vec3 0 2 0) lightTex

cornellBox :: [SomeHittable]
cornellBox =
  let
    -- Materials.
    red   = Lambertian $ SolidColour $ Vec3 0.65 0.05 0.05
    white = Lambertian $ SolidColour $ Vec3 0.73 0.73 0.73
    green = Lambertian $ SolidColour $ Vec3 0.12 0.45 0.15
    light = DiffuseLight $ SolidColour $ Vec3 15 15 15

    -- Boxes.
    box1 = map (SomeHittable . (`rotateQuad` 15)) $ makeBox (Vec3 135 0 50) (Vec3 300 165 215) white
    box2 = map (SomeHittable . (`rotateQuad` (-18))) $ makeBox (Vec3 145 0 365) (Vec3 310 330 530) white

    -- Walls.
    wall1  = SomeHittable $ makeQuad (Vec3 555 0 0) (Vec3 0 555 0) (Vec3 0 0 555) green
    wall2  = SomeHittable $ makeQuad (Vec3 0 0 0) (Vec3 0 555 0) (Vec3 0 0 555) red
    wall3  = SomeHittable $ makeQuad (Vec3 0 0 0) (Vec3 555 0 0) (Vec3 0 0 555) white
    wall4  = SomeHittable $ makeQuad (Vec3 555 555 555) (Vec3 (-555) 0 0) (Vec3 0 0 (-555)) white
    wall5  = SomeHittable $ makeQuad (Vec3 0 0 555) (Vec3 555 0 0) (Vec3 0 555 0) white
    light' = SomeHittable $ makeQuad (Vec3 343 554 332) (Vec3 (-130) 0 0) (Vec3 0 0 (-105)) light
  in
  [wall1, wall2, wall3, wall4, wall5, light'] ++ box1 ++ box2

finalScene :: Int -> Texture -> [SomeHittable]
finalScene seed texture = ground ++ [ light
                                    , movingSphere
                                    , glassSphere
                                    , metalSphere
                                    , subDialectric
                                    , subSolid
                                    , earthSphere
                                    , perlinSphere
                                    ] ++ randomSpheres
  where
    -- Light.
    lightTex = DiffuseLight $ SolidColour $ Vec3 7 7 7
    light    = SomeHittable $ makeQuad (Vec3 123 554 147) (Vec3 300 0 0) (Vec3 0 0 265) lightTex

    -- Ground.
    ground = map SomeHittable . fst $ mkGround 20 (Lambertian $ SolidColour $ Vec3 0.48 0.83 0.53) $ mkStdGen seed

    -- Moving sphere.
    center1      = Vec3 400 400 200
    delta        = Vec3 30 0 0
    sphereMat    = Lambertian $ SolidColour $ Vec3 0.7 0.3 0.1
    movingSphere = SomeHittable $ MovingSphere (Ray center1 delta 0) 50 sphereMat

    -- Glass sphere.
    glassSphere = SomeHittable $ StaticSphere (Vec3 260 150 45) 50 (Dialectric 1.5)

    -- Metal sphere.
    metalSphere = SomeHittable $ StaticSphere (Vec3 0 150 145) 50 (Metal (Vec3 0.8 0.8 0.9) 1.0)

    -- Subsurface sphere.
    subDialectric = SomeHittable $ StaticSphere (Vec3 360 150 145) 70 (Dialectric 1.5)
    subSolid      = SomeHittable $ StaticSphere (Vec3 360 150 145) 60 (Lambertian $ SolidColour $ Vec3 0.2 0.4 0.9)

    -- Earth sphere.
    earthSphere = SomeHittable $ StaticSphere (Vec3 400 200 400) 100 (Lambertian texture)

    -- Perlin sphere.
    perTex       = makePerlin seed
    perlinSphere = SomeHittable $ StaticSphere (Vec3 220 280 300) 80 (Lambertian $ NoiseTexture perTex 0.2)

    -- Random spheres.
    randomSpheres = map SomeHittable . fst $ mkSpheres 1000 (mkStdGen seed)

    mkSpheres :: Int -> StdGen -> ([Sphere], StdGen)
    mkSpheres n g = go n g []
      where
        go 0 gen spheres = (spheres, gen)
        go i gen spheres =
          let (x, gen1) = sampleFractionInRange gen (-10) 130
              (y, gen2) = sampleFractionInRange gen1 270 435
              (z, gen3) = sampleFractionInRange gen2 295 460
              center = Vec3 x y z
              radius = 10
              sphere = StaticSphere center radius (Lambertian $ SolidColour $ Vec3 0.73 0.73 0.73)
          in go (i - 1) gen3 (sphere : spheres)

    mkGround :: Int -> Material -> StdGen -> ([Quad], StdGen)
    mkGround boxesPerSide material gen =
      let (qs, finalGen) = foldl' generateBox ([], gen) [0..boxesPerSide-1]
      in (concat qs, finalGen)
      where
        w    = 100.0
        x0 i = -1000.0 + fromIntegral i*w
        z0 j = -1000.0 + fromIntegral j*w
        y0   = 0.0

        generateBox (accBoxes, currentGen) i =
          foldl' (\(acc, gen') j ->
            let xStart = x0 i
                zStart = z0 j
                xEnd   = xStart + w
                zEnd   = zStart + w

                (yEnd, newGen) = sampleFractionInRange gen' 1 101
                bottomLeft     = Vec3 xStart y0 zStart
                topRight       = Vec3 xEnd yEnd zEnd
                newBox         = makeBox bottomLeft topRight material
            in (newBox : acc, newGen)
                ) (accBoxes, currentGen) [0..boxesPerSide-1]

