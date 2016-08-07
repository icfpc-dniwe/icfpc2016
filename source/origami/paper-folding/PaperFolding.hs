module PaperFolding where -- (
--    wrapConvexHull
--  , Paper (..)
--  , Wireframe (..)
--  , Mapping (..)
--  ) where


import ConvexHull
import Control.Arrow ((***))
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
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

instance Monoid Mapping where
  mempty = Mapping []
  mappend (Mapping lhs) (Mapping rhs) = Mapping $ lhs'' ++ rhs'' where
    lhs' = nub lhs
    rhs' = nub rhs

    mhs  = map (\l -> (l, lookup (snd l) rhs')) lhs'
    lhs'' = map (\(l, m) -> (fst l, maybe (snd l) id m)) mhs

    limg = map snd lhs'
    rhs'' = filter (\r -> not $ (fst r) `elem` limg) rhs'

    
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


diSegmentEq :: Segment -> Segment -> Bool
diSegmentEq p@(Segment p1 p2) q = (p == q) || ((Segment p2 p1) == q)


performAction :: Action -> Paper -> Paper

performAction (Fold s) paper = performAction (Mirror s) . performAction (Split s) $ paper

performAction (Split l) (Paper (Wireframe ws) mapping)
  = Paper (Wireframe ws') mapping' where

    ws' = nubBy diSegmentEq (new ++ old)

    ls = map (intersectLineSegment l) ws
    ps = sortBy posOrder $ filter (belongsLineVertex l) (catMaybes ls)

    new = zipWith Segment ps (drop 1 $ cycle ps)

    splitSegment segment@(Segment p1 p2) intersection = ($ intersection) $ maybe
      ([segment], [])
      (\p -> if (p /= p1) && (p /= p2)
             then ([Segment p1 p, Segment p p2], [(p, p)])
             else ([segment], []))

    (old, ms') = concat *** concat $ (unzip $ zipWith splitSegment ws ls)
    
    mapping' = mapping <> (Mapping ms')


performAction (Mirror l) (Paper (Wireframe ws) mapping)
  = Paper (Wireframe ws') mapping' where

    (ws', ms') = unzip $ map mirrorSegment ws

    mapping' = mapping <> (Mapping $ concat ms') 

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

