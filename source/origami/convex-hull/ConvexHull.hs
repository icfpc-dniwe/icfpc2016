module ConvexHull (
    convexHull
  ) where

import Data.List (sortBy, nub)
import Linear.V2
import Linear.Metric
import Types

fix' :: (Eq a) => (a -> a) -> a -> a
fix' f x = let x' = f x in case (x == x') of
  True  -> x
  False -> fix' f x' 

posOrder :: IVertex -> IVertex -> Ordering
posOrder (V2 x1 y1) (V2 x2 y2) = let result = (compare y1 y2) in case result of
  EQ -> compare x1 x2
  _  -> result

ccwOrder :: IVertex -> IVertex -> IVertex -> Ordering
ccwOrder origin a b = compare (crossZ (a - origin) (b - origin)) 0


convexHull :: [IVertex] -> [IVertex]
convexHull = fix' convexHullStep


convexHullStep :: [IVertex] -> [IVertex]
convexHullStep [] = [] -- no pls
convexHullStep vs = convexHull' dniwe vs'' where

  dniwe :: IVertex
  dniwe = (head $ sortBy posOrder vs')

  vs' :: [IVertex]
  vs' = nub vs

  vs'' :: [IVertex]
  vs'' = (sortBy (ccwOrder dniwe) (filter (/= dniwe) vs')) ++ [dniwe]
  

convexHull' :: IVertex -> [IVertex] -> [IVertex]
convexHull' _ [] = []
convexHull' _ (v:[]) = [v]
convexHull' v0 (v:v1:vs) = case (compare (crossZ (v - v0) (v1 - v)) 0) of
  LT -> v:(convexHull' v (v1:vs))
  _  -> convexHull' v0 (v1:vs)
