module MinBoxChecker
       ( getMinBoundingBox
       , checkBoundingBox
       , v2GetVector
       , v2DotProduct
       , v2Length
       , v2GetAngle
       , v2Rotate
       , v2RotatePolygon
       , testDummy
       , testRhombus
       , testFailRhombus
       , testPolygon
       ) where

-- The smallest-area enclosing rectangle of a polygon has a side collinear with one of the edges of its convex hull.

import Types
import Linear.V2
import qualified Data.Set as S
import Data.Ratio
import Debug.Trace

v2GetVector :: (Num a) => V2 a -> V2 a -> V2 a
v2GetVector (V2 xa ya) (V2 xb yb) = V2 (xb - xa) (yb - ya)

v2DotProduct :: (Floating a) => V2 a -> V2 a -> a
v2DotProduct (V2 xa ya) (V2 xb yb) = xa * xb + ya * yb

v2Length :: (Floating a) => V2 a -> a
v2Length v = sqrt $ v2DotProduct v v

v2GetAngle :: (Floating a) => V2 a -> V2 a -> a
v2GetAngle v0 v1 = acos $ v2DotProduct v0 v1 / (v2Length v0 * v2Length v1)

v2Rotate :: (Floating a) => V2 a -> V2 a -> a -> V2 a
v2Rotate (V2 xa ya) (V2 xb yb) alpha = 
    let x = xb - xa 
        y = yb - ya
    in 
        V2 (x * cos alpha - y * sin alpha + xa) (x * sin alpha + y * cos alpha + ya)

v2RotatePolygon :: (Floating a) => [V2 a] -> V2 a -> a -> [V2 a]
v2RotatePolygon pts pvt angle = map (\p -> v2Rotate pvt p angle) pts

-- returns BL and UR min bbox corners, angle and pivot for polygon rotation
getMinBoundingBox :: (Floating a, Ord a, Show a) => Polygon a -> (V2 a, V2 a, a, V2 a)
getMinBoundingBox (Polygon ch) = 
    let pts = ch
        lines = zip pts $ drop 1 $ cycle pts
        variants = map (\(p0, p1) ->
                            let angle = v2GetAngle (v2GetVector p0 p1) (V2 0 1)
                            in
                                (v2RotatePolygon pts p0 angle, angle, p0)
                       ) lines
        bboxes = map (\(variant, angle, pvt) ->
                            let (V2 x0 y0) = variant !! 0
                            in
                                (foldl (\(minx, miny, maxx, maxy) (V2 x y) -> (min x minx, min y miny, max x maxx, max x maxy)) (x0, y0, x0, y0) variant, angle, pvt)
                     ) variants
        minbox@(area, pbl, pur, angle, pvt) = foldl1 (\b0@(area0, _, _, _, _) b1@(area1, _, _, _, _) -> if area0 < area1 then b0 else b1) $ 
                                                map (\((minx, miny, maxx, maxy), angle, pvt) -> 
                                                        let dx = maxx - minx
                                                            dy = maxy - miny
                                                        in
                                                            (dx * dy, V2 minx miny, V2 maxx maxy, angle, pvt)
                                                    ) bboxes
    in
--        trace ("pts " ++ show pts ++ " lines " ++ show lines ++ " variants " ++ show variants ++ " bboxes " ++ show bboxes ++ " minbox " ++ show minbox) $ (pbl, pur, angle, pvt)
        (pbl, pur, angle, pvt)

checkBoundingBox :: (Floating a, Ord a, Show a) => Polygon a -> Bool
checkBoundingBox ch = 
    let ((V2 xbl ybl), (V2 xur yur), angle, pvt) = getMinBoundingBox ch
        w = xur - xbl
        h = yur - ybl
    in
        w <= 1 && h <= 1

getBoundingBoxSquareFitTransform :: (Floating a, Ord a, Show a) => Polygon a -> (Bool, a, a, a, V2 a)
getBoundingBoxSquareFitTransform ch =
    let ((V2 xbl ybl), (V2 xur yur), angle, pvt) = getMinBoundingBox ch
        w = xur - xbl
        h = yur - ybl
    in
        if w <= 1 && h <= 1 then
        	let dx = if abs (xbl + w) > 1 then -xbl else 0
        	    dy = if abs (ybl + h) > 1 then -ybl else 0
        	in
        	    (True, dx, dy, angle, pvt)
        else
        	(False, 0, 0, 0, pvt)
--

testDummy :: Bool
testDummy = 
    let ch = Polygon $ [V2 (0/1) (0/1), V2 (1/1) (0/1), V2 (1/1) (1/1), V2 (0/1) (1/1)]
    in
        checkBoundingBox ch

testRhombus :: Bool
testRhombus =
	let ch = Polygon $ [V2 (1/2) (0/1), V2 (0/1) (1/2), V2 (1/2) (1/1), V2 (1/1) (1/2)]
    in
        checkBoundingBox ch

testFailRhombus :: Bool
testFailRhombus =
	let ch = Polygon $ [V2 (1/2) (-1/1), V2 (-1/1) (1/2), V2 (1/2) (2/1), V2 (2/1) (1/2)]
    in
        checkBoundingBox ch

testPolygon :: Bool
testPolygon =
	let ch = Polygon $ [V2 (2/6) (1/6), V2 (1/6) (4/6), V2 (4/6) (1/1), V2 (1/1) (5/6), V2 (5/6) (2/6)]
    in
        checkBoundingBox ch

testPolygon' :: Bool
testPolygon' =
	let ch = Polygon $ map (\(V2 x y) -> (V2 (x-3) (y+5))) [V2 (2/6) (1/6), V2 (1/6) (4/6), V2 (4/6) (1/1), V2 (1/1) (5/6), V2 (5/6) (2/6)]
    in
        checkBoundingBox ch

testPolygon'' :: (Floating a, Ord a, Show a) => (Bool, a, a, a, V2 a)
testPolygon'' =
	let ch = Polygon $ map (\(V2 x y) -> (V2 (x-3) (y+5))) [V2 (2/6) (1/6), V2 (1/6) (4/6), V2 (4/6) (1/1), V2 (1/1) (5/6), V2 (5/6) (2/6)]
    in
        getBoundingBoxSquareFitTransform ch