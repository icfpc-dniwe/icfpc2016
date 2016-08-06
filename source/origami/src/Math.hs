module Math where

import Linear.V2
import Types


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

