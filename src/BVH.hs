module BVH where
import           AABB      (AABB (..), enclosingAABB, longestAxis, boundingBoxMin, hitAABB)
import           Data.List (sortBy)
import           Hittable  (Hittable (..), Object, boundingBox, Hit (..))
import Interval (Interval(..), interval)
import Control.Applicative ((<|>))

data BVHNode = BVHLeaf Object AABB            -- Leaf node.
             | BVHBranch BVHNode BVHNode AABB -- Internal node containing two children.

buildBVH :: Hittable a => [a] -> BVHNode
buildBVH objects =
    let bbox     = foldr (enclosingAABB . boundingBox) AABBEmpty objects
        axisFunc = longestAxis bbox
        sorted   = sortBy (\a b -> compare (axisFunc (boundingBoxMin (boundingBox a)))
                                           (axisFunc (boundingBoxMin (boundingBox b)))) objects
        mid   = length sorted `div` 2 
        left  = buildBVH (take mid sorted) 
        right = buildBVH (drop mid sorted)       
        final = enclosingAABB (boundingBox left) (boundingBox right)                                   
    in BVHBranch left right final 

instance Hittable BVHNode where 
    hit (BVHLeaf obj bbox) ray tRange 
        | hitAABB bbox ray tRange = obj ray tRange 
        | otherwise               = Nothing
    hit (BVHBranch left right bbox) ray tRange 
        | not (hitAABB bbox ray tRange) = Nothing
        | otherwise = 
            let leftHit  = hit left ray tRange 
                newTMax  = maybe (iMax tRange) hitT leftHit 
                rightHit = hit right ray (interval (iMin tRange) newTMax) 
            in rightHit <|> leftHit  

    boundingBox (BVHLeaf _ bbox)     = bbox 
    boundingBox (BVHBranch _ _ bbox) = bbox 
