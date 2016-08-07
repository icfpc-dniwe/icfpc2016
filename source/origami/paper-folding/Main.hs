{-# LANGUAGE ViewPatterns #-}

import ConvexHull
import PaperFolding
import Math       
import Data.List
import Data.Monoid
import Types
import Linear.V2
import System.Random
import Debug.Trace
import Graphics.Gloss.Interface.Pure.Display hiding (Polygon, color)
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Interface.Pure.Display as G


maxVertices :: Int
maxVertices = 10

fmtSegment :: Segment -> String
fmtSegment (Segment a b) = "[" ++ (fmtVR a) ++ "; " ++ (fmtVR b) ++ "]" 

pointV2 :: (Real a) => V2 a -> Point
pointV2 (fmap (fromRational . toRational) -> V2 x y) = (x, y)

vertexPicture :: Color -> VR -> Picture
vertexPicture color (V2 x y) = Translate (fromRational x) (fromRational y) $ Color color $ Circle 0.01

segmentPicture :: Color -> Segment -> Picture
segmentPicture color (Segment a b) = Color color $ Line [pointV2 a, pointV2 b]

(<$$>) f mmx = (f <$>) <$> mmx


randomHull :: Int -> IO [VR]
randomHull n = reverse . convexHull . (take n) . (randomRs (V2 (1/4) (1/4), V2 (3/4) (3/4))) <$> getStdGen

mkHullEdges :: [VR] -> [Segment]
mkHullEdges hs = zipWith Segment hs (drop 1 $ cycle hs) 

notSoRandomHull :: Int -> IO [VR]
notSoRandomHull _ = return $ foldl1 (.) [
    (+ (1/2))
  , (* (1/3))
  ] <$$> [
    V2 0 (-1)
  , V2 (1/2) 0
  , V2 0 1
  , V2 (-1) 0 
  ]

type World = (Paper, [Action])


update :: World -> World
update (paper, []) = (paper, [])
update (paper, (a:as)) = let
  paper'@(Paper _ (Mapping ms)) = performAction a paper
  ms' = filter (\(a, b) -> a /= b) ms
  in (paper', as)


render :: [Segment] -> World -> Picture
render hull ((Paper (Wireframe ws) (Mapping ms)), _)
  = Translate (-200) (-200)
  $ Scale 400 400
  $ Pictures
  $ concat [
      map (segmentPicture blue) ws
    , map (segmentPicture red) hull
    -- , map (vertexPicture green) (concat $ map (\(Segment p1 p2) -> [p1, p2]) ws)
    , map (segmentPicture yellow) (map (\(a, b) -> Segment a b) ms) 
    , map (vertexPicture yellow) $ map fst ms ++ map snd ms
    ]


main :: IO ()
main = do
  hull <- randomHull 6
  let hullEdges = mkHullEdges hull
  let actions = map Fold (cycle hullEdges)

  play
    (InWindow "visualize" (640, 480) (100, 0))
    black
    1
    (initialPaper, actions)
    (render hullEdges)
    (\evt world -> case evt of
       (EventKey (SpecialKey KeySpace) Down _ _) -> update world
       _                                         -> world)
    (\_ world -> world)


