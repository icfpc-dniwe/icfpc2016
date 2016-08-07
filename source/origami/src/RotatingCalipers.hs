-- The smallest-area enclosing rectangle of a polygon has a side collinear with one of the edges of its convex hull.

import Types
import qualified Data.Set as S

type ConvexHull = S.Set VR

v2GetVector :: V2 a -> V2 a -> V2 a
v2GetVector (V2 xa ya) (V2 xb yb) = V2 (xb - xa) (yb - ya)

v2DotProduct :: V2 a -> V2 a -> a
v2DotProduct (V2 xa ya) (V2 xb yb) = (ya - xa) ^^ 2 + (yb - xb) ^^ 2

v2Length :: V2 a -> Double
v2Length v = sqrt $ v2DotProduct v v

v2GetAngle :: V2 a -> V2 a -> Double
v2GetAngle v0 v1 = acos $ v2DotProduct v0 v1 / (v2Length v0 * v2Length v1)

v2Rotate :: V2 a -> V2 a -> Double -> V2 Double
v2Rotate (V2 xa xb) (V2 ya yb) alpha = 
	let x = xb - xa,
		y = yb - ya
	in
		V2 (x * cos alpha - y * sin alpha + xa) (x * sin alpha + y * cos alpha + ya)

v2RotatePolygon :: [V2 a] -> V2 a -> Double -> [V2 Double]
v2RotatePolygon pts pvt angle = map (\p -> v2Rotate pvt p angle)

getMinBoundingBox :: ConvexHull -> (Double, Double, Double, V2 a)
getMinBoundingBox ch = 
	let pts = S.toList ch,
		lines = zip pts $ drop 1 $ cycle pts
		variants = map (\(p0, p1) ->
					    	let angle = v2GetAngle (v2GetVector p0 p1) (V2 0 1)
							in
								(v2RotatePolygon pts p0 angle, angle, p0)
					   ) lines,
		bboxes = map (\(var, angle, pvt) ->
							let (V2 x0 y0) = var !! 0
							in
								(foldl (\(V2 x y) (minx, miny, maxx, maxy) -> (min x minx, min y miny, max x maxx, max x maxy)) (x0, y0, x0, y0) var, angle, pvt)
					 ) variants,
		minbox@(area, dx, dy, angle, pvt) = foldl1 (\b0@(area0, _, _, _, _) b1@(area1, _, _, _, _) -> if area0 < area1 then b0 else b1) $ 
												map (\((minx, miny, maxx, maxy), angle, pvt) -> 
														let dx = maxx - minx,
															dy = maxy - miny
														in
															(dx * dy, dx, dy, angle, pvt)
													) bboxes
	in
		(dx, dy, angle, pvt)

checkBoundingBox :: ConvexHull -> Bool
checkBoundingBox ch = 
	let (w, h, angle, pvt) = getMinBoundingBox ch
	in
		w <= 1 && h <= 1