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
mergeMappings = error "TODO mergeMapping"



performAction :: Action -> Paper -> Paper


performAction (Fold s) paper = performAction (Mirror s) . performAction (Split s) $ paper


performAction (Split line) (Paper (Wireframe ws) mapping)
  = Paper (Wireframe ws') mapping where

    ws' = concat $ map (splitSegment line) ws

    splitSegment l segment@(Segment p1 p2) = ($ (intersectLineSegment l segment)) $ maybe
      [segment]
      (\p -> if (p /= p1) && (p /= p2)
             then [Segment p1 p, Segment p p2]
             else [segment])
                  
  
performAction (Mirror l) (Paper (Wireframe ws) mapping)
  = Paper (Wireframe ws') mapping' where

    (ws', ms') = unzip $ map (mirrorSegment l) ws

    mapping' = mapping `mergeMappings` (Mapping ms')

    mirrorSegment line (Segment p1 p2) = let
      (

    mirrorVertex line p@(V2 x y) =
      if belongsLineVertex line p
      then (p, Nothing)
      else (p', Just (p, p')) where
        p' = p ... -- TODO

-- performAction
--   (Paper (Wireframe ws) (Mapping ms))
--   (Fold line@(Segment p1 p2)) = Paper (Wireframe ws'') (error "TODO") where
--     ws'         = line `splitSegments` ws
--     (ws'', ms') = line `mirrorSegments` ws
--     ms''        = ms `mergeMapping` ms'
  


      



  


 -- | [VR] supposed to be a convex hull, ccw ordered
wrapConvexHull :: [VR] -> Paper
wrapConvexHull hs = foldl' (flip performAction) initialPaper actions' where
  actions = zipWith (\h1 h2 -> Fold (Segment h1 h2)) (cycle hs) (drop 1 $ cycle hs)
  actions' = take (length hs) actions -- for safety sake

