module PaperFolding where -- (
--    wrapConvexHull
--  , Paper (..)
--  , Wireframe (..)
--  , Mapping (..)
--  ) where


import ConvexHull
import Data.Foldable
import Data.List (sortBy, nub)
import Data.Maybe
import Debug.Trace
import Linear.V2
import Math
import Types


data Wireframe = Wireframe [Segment]

initialWireframe :: Wireframe
initialWireframe = Wireframe [
    Segment (V2 0 0) (V2 1 0)
  , Segment (V2 1 0) (V2 1 1)
  , Segment (V2 1 1) (V2 0 1)
  , Segment (V2 0 1) (V2 0 0)
  ]

  
data Mapping = Mapping [(VR, VR)]

initialMapping :: Mapping
initialMapping = Mapping $ map (\v -> (v, v)) [
    V2 0 0
  , V2 1 0
  , V2 1 1
  , V2 0 1
  ]


data Paper = Paper Wireframe Mapping

initialPaper :: Paper
initialPaper = Paper initialWireframe initialMapping


data Action = Split Segment
            | Mirror Segment
            | Fold Segment
            | Move VR


mergeMappings :: Mapping -> Mapping -> Mapping 
mergeMappings (Mapping ms) (Mapping ms') = Mapping (ms ++ ms')



performAction :: Action -> Paper -> Paper

performAction (Fold s) paper = performAction (Mirror s) . performAction (Split s) $ paper


performAction (Split l) (Paper (Wireframe ws) mapping)
  = Paper (Wireframe ws') mapping where

    ws' = new ++ old

    ls = map (intersectLineSegment l) ws
    ps = sortBy posOrder $ filter (belongsLineVertex l) (catMaybes ls)

    new = zipWith Segment ps (drop 1 $ cycle ps)

    splitSegment segment@(Segment p1 p2) intersection = ($ intersection) $ maybe
      [segment]
      (\p -> if (p /= p1) && (p /= p2)
             then [Segment p1 p, Segment p p2]
             else [segment])

    old = concat $ zipWith splitSegment ws ls
    
  
performAction (Mirror l) (Paper (Wireframe ws) mapping)
  = Paper (Wireframe ws') mapping' where

    (ws', ms') = unzip $ map mirrorSegment ws

    mapping' = mapping `mergeMappings` (Mapping $ concat ms')

    mirrorSegment (Segment p1 p2) = let
      (p1', m1) = mirrorVertex p1
      (p2', m2) = mirrorVertex p2
      in (Segment p1' p2', catMaybes [m1, m2])

    mirrorVertex p =
      if GT == sideLineVertex l p
      then (p, Nothing)
      else (p', Just (p, p')) where p' = mirrorLineVertex l p 

  
 -- | [VR] supposed to be a convex hull, ccw ordered
wrapConvexHull :: [VR] -> Paper
wrapConvexHull hs = foldl' (flip performAction) initialPaper actions' where
  actions = zipWith (\h1 h2 -> Fold (Segment h1 h2)) (cycle hs) (drop 1 $ cycle hs)
  actions' = take (length hs) actions -- for safety sake

