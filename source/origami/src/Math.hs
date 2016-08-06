module Math where

import Linear.V2

import Types

clockwise :: Polygon -> Bool
clockwise (Polygon pts) = sum (zipWith edge pts (tail pts ++ [head pts])) >= 0
  where edge (V2 x1 y1) (V2 x2 y2) = (x2 - x1) * (y2 + y1)
