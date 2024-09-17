{- |
Module      :  BVH
Copyright   :  (c) Quinn Anhorn 2024
License     :  BSD3 (see LICENSE)
Maintainer  :  qda869@usask.ca
Stability   :  experimental

This module defines the `BVHNode` data type and its instance of the `Hittable` class.
A Bounding Volume Hierarchy (BVH) is used to accelerate ray-object intersection 
tests by organizing objects in a binary tree structure. Each node in the BVH contains 
a bounding box that encapsulates its child nodes or objects, allowing for efficient 
intersection tests by quickly excluding large portions of the scene when tracing rays.

The BVH structure is a crucial optimization for rendering complex scenes in ray tracing,
reducing the number of intersection tests required.
-}

module BVH (BVHNode(..),
            buildBVH,
            hit) where

import           AABB                (AABB (..), boundingBoxMin, enclosingAABB,
                                      hitAABB, longestAxis)
import           Control.Applicative ((<|>))
import           Data.List           (sortBy)
import           Hittable            (Hit (..), Hittable (..),
                                      SomeHittable (..), boundingBox)
import           Interval            (Interval (..), interval)

data BVHNode = BVHLeaf !SomeHittable !AABB       -- Leaf node.
             | BVHBranch !BVHNode !BVHNode !AABB -- Internal node containing two children.

buildBVH :: [SomeHittable] -> BVHNode
buildBVH [hittable@(SomeHittable obj)] = BVHLeaf hittable (boundingBox obj)
buildBVH objects =
    let bbox     = foldr (\(SomeHittable obj) acc -> enclosingAABB (boundingBox obj) acc) AABBEmpty objects
        axisFunc = longestAxis bbox
        sorted   = sortBy (\(SomeHittable a) (SomeHittable b) ->
                           compare (axisFunc (boundingBoxMin (boundingBox a)))
                                   (axisFunc (boundingBoxMin (boundingBox b)))) objects
        mid   = length sorted `div` 2
        left  = buildBVH (take mid sorted)
        right = buildBVH (drop mid sorted)
        final = enclosingAABB (boundingBox left) (boundingBox right)
    in BVHBranch left right final

instance Hittable BVHNode where
    hit (BVHLeaf (SomeHittable obj) bbox) ray tRange
        | hitAABB bbox ray tRange = hit obj ray tRange
        | otherwise               = Nothing
    hit (BVHBranch left right bbox) ray tRange
        | not (hitAABB bbox ray tRange) = Nothing
        | otherwise =
            case hit left ray tRange of
                Just leftHit ->
                    let newTMax  = hitT leftHit
                        rightHit = hit right ray (interval (iMin tRange) newTMax)
                    in rightHit <|> Just leftHit
                Nothing ->
                    hit right ray tRange

    boundingBox (BVHLeaf _ bbox)     = bbox
    boundingBox (BVHBranch _ _ bbox) = bbox
