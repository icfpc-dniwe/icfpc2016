-- The smallest-area enclosing rectangle of a polygon has a side collinear with one of the edges of its convex hull.

import Types
import qualified Data.Set as S

type ConvexHull = S.Set VR

v2DotProduct :: VR -> VR -> Rational
v2DotProduct (V2 xa ya) (V2 xb yb) = (ya - xa) ^^ 2 + (yb - xb) ^^ 2

v2Length :: VR -> Double
v2Length v = sqrt $ v2DotProduct v v

v2GetAngle :: VR -> VR -> Double
v2GetAngle v0 v1 = acos $ v2DotProduct v0 v1 / (v2Length v0 * v2Length v1)

getBoundingBox :: ConvexHull -> (Rational, Rational)
getBoundingBox ch = 
	let pts = S.toList ch,
		lines = zip pts $ drop 1 $ cycle pts,
		