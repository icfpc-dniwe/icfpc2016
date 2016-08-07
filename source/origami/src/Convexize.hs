module Convexize
       ( convexize
       ) where

import Control.Arrow
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear
import Data.Graph.Inductive.Graph (Node, Edge, DynGraph)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Basic as G
import qualified Data.Graph.Inductive.PatriciaTree as G

import Debug.Trace

import Types

type SkelGraph gr a = gr (V2 a) ()

linesGraph :: (Ord a, DynGraph gr) => [Segment a] -> SkelGraph gr a
linesGraph segs = G.undir $ G.mkGraph (zip [0..] points) edges
  where points = nub $ concatMap (\(Seg a b) -> [a, b]) segs
        pmap = M.fromList $ zip points [0..]
        edges = map (\(Seg a b) -> (pmap M.! a, pmap M.! b, ())) segs

getPolys :: (Num a, Ord a, DynGraph gr) => SkelGraph gr a -> Set (Convex a)
getPolys gr
  | G.isEmpty gr = S.empty
  | otherwise =
    let (start, next, _) = head $ G.labEdges gr
        (gr', mpoly) = walkPoly gr start (start, next)
        npolys = maybe S.empty (S.singleton . Convex) mpoly
    in npolys `S.union` getPolys gr'

walkPoly :: (Num a, Ord a, DynGraph gr) => SkelGraph gr a -> Node -> Edge -> (SkelGraph gr a, Maybe (Set (V2 a)))
walkPoly gr start edge@(from, to)
  | to == start = (gr', Just $ S.singleton pto)
  | null pts = (gr', Nothing)
  | otherwise = second (S.insert pto <$>) $ walkPoly gr' start (to, next)
                
  where gr' = G.delEdge edge gr

        Just pfrom = G.lab gr' from
        Just pto = G.lab gr' to

        v1 = pto - pfrom

        angleFrom pnext =
          let v2 = pnext - pfrom
          in v1 `crossZ` v2
        pts = map (\n -> (n, angleFrom $ fromJust $ G.lab gr' n)) $ G.suc gr' to
        next = fst $ minimumBy (comparing snd) pts

convexize :: Skeleton -> ConvSkeleton
convexize (Skeleton skel) = ConvSkeleton $ getPolys (linesGraph skel :: G.Gr VR ())
