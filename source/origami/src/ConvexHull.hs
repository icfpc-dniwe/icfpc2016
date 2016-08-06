module ConvexHull (
    convexHull
  , posOrder
  , ccwOrder
  ) where

import Data.List (sortBy, nub)
import Linear.V2
import Types


fix' :: (Eq a) => (a -> a) -> a -> a
fix' f x = let x' = f x in case (x == x') of
  True  -> x
  False -> fix' f x' 


posOrder :: VR -> VR -> Ordering
posOrder (V2 x1 y1) (V2 x2 y2) = let result = (compare y1 y2) in case result of
  EQ -> compare x1 x2
  _  -> result


ccwOrder :: VR -> VR -> VR -> Ordering
ccwOrder origin a b = compare (crossZ (a - origin) (b - origin)) 0


convexHull :: [VR] -> [VR]
convexHull = fix' convexHullStep


convexHullStep :: [VR] -> [VR]
convexHullStep [] = [] -- no pls
convexHullStep vs = convexHullStep' dniwe vs'' where

  dniwe :: VR
  dniwe = (head $ sortBy posOrder vs')

  vs' :: [VR]
  vs' = nub vs

  vs'' :: [VR]
  vs'' = (sortBy (ccwOrder dniwe) (filter (/= dniwe) vs')) ++ [dniwe]
  

convexHullStep' :: VR -> [VR] -> [VR]
convexHullStep' _ [] = []
convexHullStep' _ (v:[]) = [v]
convexHullStep' v0 (v:v1:vs) = case (compare (crossZ (v - v0) (v1 - v)) 0) of
  LT -> v:(convexHullStep' v (v1:vs))
  _  -> convexHullStep' v0 (v1:vs)
