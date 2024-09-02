{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Image
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module provides functionality for rendering images in a ray tracing
context. The `Image` data type represents a rendered image with pixel
colors, while the `renderImage` function handles the ray tracing process
to produce an image based on the provided camera and world. The module
also includes utilities for saving the rendered image in PPM format via
`writeImage` and converting an `Image` to the PPM format with `image2PPM`.
Parallel processing is utilized in the rendering process to enhance
performance.
-}

module Image where

import           Camera                      (Camera (Camera, camImageHeight, camImageWidth, camSamplesPerPixel),
                                              getRay, rayColour)
import           Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import           Data.Text                   (Text)
import           Data.Text.IO                (writeFile)
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Builder      as TB
import           Hittable                    (World)
import           Interval                    (Interval (Interval), clamp)
import           Math                        (linear2Gamma, R)
import           Random                      (sampleFraction)
import           System.Directory            (createDirectoryIfMissing,
                                              renameFile)
import           System.Random               (RandomGen, mkStdGen)
import           Text.Printf                 (printf)
import           Utils                       (getSecondsNow)
import           Vec3                        (RGB, Vec3 (..), (^*), (^+^))

data Image = Image { imageWidth   :: !Int
                   , imageHeight  :: !Int
                   , imageColours :: ![RGB]}

renderImage :: Camera -> World -> Int -> Image
renderImage camera@(Camera { camImageWidth = width, camImageHeight = height, camSamplesPerPixel = samples }) world depth =
    let coords  = [(a, b) | a <- [00..height-1], b <- [0..width-1]]
        colours = map computeColour coords `using` parListChunk 128 rdeepseq
    in Image width height colours
  where
    computeColour (j, i) = avgColour
      where
        g = mkStdGen (i * width + j)
        (r1, g1) = sampleFraction g
        (r2, _)  = sampleFraction g1
        baseU = (fromIntegral i + r1) / fromIntegral (width - 1)
        baseV = (fromIntegral j + r2) / fromIntegral (height - 1)
        avgColour = accumulateColour g samples (Vec3 0 0 0) baseU baseV

    accumulateColour :: RandomGen g => g -> Int -> RGB -> R -> R -> RGB
    accumulateColour _ 0 accColour _ _ = accColour ^* (1 / fromIntegral samples)
    accumulateColour gen remainingSamples accColour baseU baseV = 
        let (ray, g') = getRay camera baseU baseV gen
            colour    = rayColour g' depth world ray
        in accumulateColour g' (remainingSamples - 1) (accColour ^+^ colour) baseU baseV

writeImage :: Camera -> World -> Int -> IO ()
writeImage camera world raysPerSample = do
    createDirectoryIfMissing True "images"
    
    start <- getSecondsNow
    let image = renderImage camera world raysPerSample
        tmpFilename = "images/tmp.ppm"
        filename = printf "images/%d-%d-%d-%d.ppm"
                         (camImageWidth camera)
                         (camImageHeight camera)
                         (camSamplesPerPixel camera)
                         raysPerSample

    saveImage tmpFilename $ image2PPM image
    renameFile tmpFilename filename
    
    end <- getSecondsNow
    let elapsed = end - start
        pixelRate = fromIntegral (camImageHeight camera * camImageWidth camera) / elapsed

    putStrLn $        "Image created: " ++ filename
    putStrLn $ printf "Total time elapsed: %.3f seconds" elapsed
    putStrLn $ printf "Pixels processed per second: %.3f\n" pixelRate

image2PPM :: Image -> Text
image2PPM (Image width height colours) =
    TL.toStrict $ TB.toLazyText (header <> body colours)
    where
        header = TB.fromText "P3\n"
              <> TB.fromString (show width)
              <> TB.fromText " "
              <> TB.fromString (show height)
              <> TB.fromText "\n255\n"
        body   = foldMap writeColour
        writeColour (Vec3 r g b) =
            TB.fromString $ printf "%d %d %d\n" (scale r) (scale g) (scale b)
            where
              intensity = Interval 0.000 0.999
              scale :: Double -> Int
              scale c = round (256 * clamp intensity (linear2Gamma c))

saveImage :: FilePath -> Text -> IO ()
saveImage path content = Data.Text.IO.writeFile path (content <> "\n")
