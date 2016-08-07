module Convexize
       ( convexize
       , test
       ) where

import Data.Maybe
import Data.Monoid
import Data.List
import Data.Ord
import Control.Arrow
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

getPolys :: (Show a, Real a, DynGraph gr) => SkelGraph gr a -> [Convex a]
getPolys gr = getPolys' gr S.empty

getPolys' :: (Show a, Real a, DynGraph gr) => SkelGraph gr a -> Set (Set (V2 a)) -> [Convex a]
getPolys' gr existing
  | null edges = []
  | otherwise = case mpoly of
    Just poly@(S.fromList -> polypts) | not (polypts `S.member` existing) -> Convex poly : getPolys' gr' (S.insert polypts existing)
    _ -> getPolys' gr' existing

  where edges = G.edges gr
        e@(start, _) = head edges
        (gr', mpoly) = fromMaybe (G.delEdge e gr, Nothing) $ walkPoly gr start e EQ

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational

walkPoly :: (Show a, Real a, DynGraph gr) => SkelGraph gr a -> Node -> Edge -> Ordering -> Maybe (SkelGraph gr a, Maybe [V2 a])
walkPoly gr0 start edge@(from, to) ord
  | to == start = Just (gr', Just [pto])
  | null pts = Just (gr', Nothing)
  | null pts' = Nothing
  | otherwise = second (fmap (pto :)) <$> walkPoly gr' start (to, next) nord
                
  where gr' = G.delEdge edge gr0

        Just pfrom = G.lab gr' from
        Just pto = G.lab gr' to

        v1 = normalize $ fmap toDouble $ pto - pfrom

        angleCos pnext =
          let v2 = normalize $ fmap toDouble $ pnext - pto
          in (v1 `dot` v2, ((v1 `crossZ` v2) `compare` 0) <> ord)
        pts = traceShowId $ map (\n -> ((n, angleCos $ fromJust $ G.lab gr' n), fromJust $ G.lab gr' n)) $ G.suc gr' to
        pts'
          | ord == EQ = pts
          | otherwise = filter (\((_, (_, c)), _) -> c == ord) pts
        ((next, (_, nord)), _) = maximumBy (comparing (fst . snd . fst)) pts'

convexize :: Skeleton -> ConvSkeleton
convexize (Skeleton skel) = ConvSkeleton $ getPolys (linesGraph skel :: G.Gr VR ())

test :: Skeleton -> Skeleton
test (Skeleton skel) = Skeleton $ testConv (linesGraph skel :: G.Gr VR ())
  where testConv gr = map conv $ G.labEdges gr
          where conv (from, to, _) = Seg (fromJust (G.lab gr from)) (fromJust (G.lab gr to))
