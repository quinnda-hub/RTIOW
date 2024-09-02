{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Vec3

epsilon :: Double
epsilon = 1e-10

closeEnough :: Vec3 -> Vec3 -> Bool
closeEnough (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  abs (x1 - x2) < epsilon &&
  abs (y1 - y2) < epsilon &&
  abs (z1 - z2) < epsilon

instance Arbitrary Vec3 where
  arbitrary = Vec3 <$> arbitrary <*> arbitrary <*> arbitrary

-- Property: Addition is commutative
prop_addComm :: Vec3 -> Vec3 -> Bool
prop_addComm v1 v2 = (v1 ^+^ v2) == (v2 ^+^ v1)

-- Property: Addition is associative
prop_addAssoc :: Vec3 -> Vec3 -> Vec3 -> Bool
prop_addAssoc v1 v2 v3 = closeEnough ((v1 ^+^ v2) ^+^ v3) (v1 ^+^ (v2 ^+^ v3))

-- Property: Subtraction of a vector from itself should yield the zero vector
prop_subSelf :: Vec3 -> Bool
prop_subSelf v = (v ^-^ v) == zeroV

-- Property: Scalar multiplication and division should cancel out
prop_scaleAndDivide :: Vec3 -> Double -> Property
prop_scaleAndDivide v c = c /= 0 ==> closeEnough ((v ^* c) ^/ c) v

-- Property: Magnitude of a normalized vector should be close to 1
prop_magnitudeNormalize :: Vec3 -> Property
prop_magnitudeNormalize v = v /= zeroV ==> abs (magnitude (normalize v) - 1) < epsilon

-- Property: Cross product of a vector with itself should be the zero vector
prop_crossSelf :: Vec3 -> Bool
prop_crossSelf v = (v Vec3.>< v) == zeroV

-- Property: Normalizing a zero vector should return the zero vector
--prop_normalizeZero :: Bool
--prop_normalizeZero = normalize zeroV == zeroV

-- Property: Dot product is commutative
prop_dotComm :: Vec3 -> Vec3 -> Bool
prop_dotComm v1 v2 = (v1 <.> v2) == (v2 <.> v1)

-- Property: Scalar multiplication distributes over vector addition
prop_distributive :: Vec3 -> Vec3 -> Double -> Bool
prop_distributive v1 v2 c = closeEnough (v1 ^* c ^+^ v2 ^* c) ((v1 ^+^ v2) ^* c)

-- Property: Zero vector is the additive identity
prop_addZero :: Vec3 -> Bool
prop_addZero v = (v ^+^ zeroV) == v

-- Property: Scalar multiplication by 1 should yield the same vector
prop_scaleByOne :: Vec3 -> Bool
prop_scaleByOne v = (v ^* 1) == v

-- Run all tests
return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  allTestsPassed <- runTests
  if allTestsPassed
    then putStrLn "All tests passed."
    else putStrLn "Some tests failed."
