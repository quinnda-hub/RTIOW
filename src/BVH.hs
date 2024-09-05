module BVH where
import           AABB      (AABB (..), enclosingAABB, longestAxis, boundingBoxMin, hitAABB)
import           Data.List (sortBy)
import           Hittable  (Hittable (..), boundingBox, Hit (..), SomeHittable (..))
import Interval (Interval(..), interval)
import Control.Applicative ((<|>))

data BVHNode = BVHLeaf !SomeHittable !AABB            -- Leaf node.
             | BVHBranch !BVHNode !BVHNode !AABB -- Internal node containing two children.

buildBVH :: [SomeHittable] -> BVHNode
buildBVH [] = undefined
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
