-- The smallest-area enclosing rectangle of a polygon has a side collinear with one of the edges of its convex hull.

import Types
import qualified Data.Set as S

type ConvexHull = S.Set VR

max' :: (VR, Double) -> (VR, Double) -> (VR, Double)
max' t0 t1 = if snd t0 > snd t1 then t0 else t1

getBoundingBox :: ConvexHull -> (Rational, Rational)
getBoundingBox ch = 
	let pts = S.toList ch,
		lines = zip pts $ drop 1 $ cycle pts,
