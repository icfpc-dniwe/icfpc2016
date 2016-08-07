module Math where

import Linear.V2
import Linear.Epsilon
import Types
import Debug.Trace

clockwise :: Polygon -> Bool
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


belongsLineVertex :: Segment -> VR -> Bool
belongsLineVertex
  (Segment (V2 x1 y1) (V2 x2 y2))
  (V2 x y) = let
    dx  = x2 - x1
    dy  = y2 - y1
    dx1 = x - x1
    dy1 = y - y1
    in (nearZero $ dx * dy1 - dx1 * dy) 


belongsLineSegment :: Segment -> Segment -> Bool
belongsLineSegment segment (Segment q1 q2)
  = all (belongsLineVertex segment) [q1, q2]
  

       
intersectSegmentVertex
  :: Segment
  -> VR
  -> Maybe VR

intersectSegmentVertex
  (Segment (V2 x1 y1) (V2 x2 y2))
  p@(V2 x y) = let
    dx  = x2 - x1
    dy  = y2 - y1
    dx1 = x - x1
    dy1 = y - y1
    dx2 = x2 - x
    dy2 = y2 - y

    s1 = dx * dx1 + dy * dy1
    s2 = dx * dx2 + dy * dy2

    s = s1 * s2
    b = dx * dy1 - dx1 * dy

    in if (nearZero b) && (s >= 0)
       then Just p
       else Nothing


intersectLineSegment
  :: Segment
  -> Segment
  -> Maybe VR

intersectLineSegment line segment
  = intersectLineLine line segment >>= intersectSegmentVertex segment
