module Math
       ( clockwise
       , ketTri
       , intersectLineLine
       ) where

import Data.List
import Linear.V2
import Types

clockwise :: (Num a, Ord a) => Polygon a -> Bool
clockwise (Polygon pts) = sum (zipWith edge pts (tail pts ++ [head pts])) >= 0
  where edge (V2 x1 y1) (V2 x2 y2) = (x2 - x1) * (y2 + y1)

intersectLineLine 
  :: Segment
  -> Segment
  -> Maybe VR

intersectLineLine
 (Segment (V2 x1 y1) (V2 x2 y2))
 (Segment (V2 x3 y3) (V2 x4 y4)) = let
  dx12 = x1 - x2
  dx34 = x3 - x4
  dy12 = y1 - y2
  dy34 = y3 - y4
  den = dx12 * dy34 - dy12 * dx34

  in if den == 0
     then Nothing
     else let
       det12 = x1*y2 - y1*x2
       det34 = x3*y4 - y3*x4 
       numx = det12 * dx34 - dx12 * det34
       numy = det12 * dy34 - dy12 * det34
       in Just $ V2 (numx / den) (numy / den)

ketTri :: (Ord a, Num a) => Polygon a -> [Triangle a]
ketTri (Polygon ps@(p1:p2:p3:qs)) = scan vs stack rs
  where vs = qs ++ [p1]
        stack = [p3, p2, p1, last ps]
        rs = reflexVertices ps
ketTri (Polygon _) = error "ketTri: less than 3 edges"

scan :: (Ord a, Num a) => [V2 a] -> [V2 a] -> [V2 a] -> [Triangle a]
scan [v] [x_p, x_i, _, _] _ = [Tri x_i x_p v]
scan (v:vs) ss@[_,_,_] rs = scan vs (v:ss) rs
scan vs@(v:vs') ss@(x_p:x_i:ss'@(x_m:x_mm:_)) rs
  | isEar rs mtri = mtri : scan vs (x_p:ss') rs'
  | otherwise = scan vs' (v:ss) rs
  where mtri = Tri x_m x_i x_p
        rs' = rs \\ (isConvex (Tri x_m x_p v) ++ isConvex (Tri x_mm x_m x_p))
        isConvex tri@(Tri _ i _) = if isLeftTurn tri then [i] else []
scan _ _ _ = []

isEar :: (Ord a, Num a) => [V2 a] -> Triangle a -> Bool
isEar [] _ = True
isEar rs tri = isLeftTurn tri
               && not (any (tri `containsBNV`) rs)

reflexVertices :: (Ord a, Num a) => [V2 a] -> [V2 a]
reflexVertices xs = [ x | tri@(Tri _ x _) <- angles xs, isRightTurnOrOn tri ]

containsBNV :: (Num a, Ord a) => Triangle a -> V2 a -> Bool
containsBNV (Tri s t v) p = a == b && b == c
  where a = isLeftTurn (Tri s t p)
        b = isLeftTurn (Tri t v p)
        c = isLeftTurn (Tri v s p)

angles :: [V2 a] -> [Triangle a]
angles xs = zipWith3 Tri (rotateR xs) xs (rotateL xs)

rotateL :: [a] -> [a]
rotateL xs = tail xs ++ [head xs]

rotateR :: [a] -> [a]
rotateR xs = last xs : init xs

isRightTurnOrOn :: (Num a, Ord a) => Triangle a -> Bool
isRightTurnOrOn tri = area2 tri <= 0

isLeftTurn :: (Num a, Ord a) => Triangle a -> Bool
isLeftTurn tri = area2 tri > 0

area2 :: (Num a) => Triangle a -> a
area2 (Tri (V2 x2 y2) (V2 x0 y0) (V2 x1 y1)) = (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)
