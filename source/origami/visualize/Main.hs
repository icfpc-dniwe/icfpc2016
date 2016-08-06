import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString.Lazy as P
import Graphics.Gloss.Interface.Pure.Display hiding (Polygon, color)
import qualified Graphics.Gloss.Interface.Pure.Display as G
import Linear.V2

import Types
import Parser
import Math

pointV2 :: VR -> Point
pointV2 (fmap fromRational -> V2 x y) = (x, y)

polygonPicture :: Polygon -> Picture
polygonPicture (Polygon pol) = G.Polygon $ map pointV2 pol

silhouettePicture :: Silhouette -> Picture
silhouettePicture (Silhouette silh) = Pictures $ map polygonize silh
  where polygonize pol = Color color $ polygonPicture pol
          where color
                  | clockwise pol = white
                  | otherwise = red

segmentPicture :: Segment -> Picture
segmentPicture (Segment a b) = Color color $ Line [pointV2 a, pointV2 b]
  where color = blue

skeletonPicture :: Skeleton -> Picture
skeletonPicture (Skeleton skel) = Pictures $ map segmentPicture skel

problemPicture :: Problem -> Picture
problemPicture (Problem silh skel) = Pictures [silhouettePicture silh, skeletonPicture skel]

visualize :: FilePath -> IO ()
visualize probPath = do
  file <- BL.readFile probPath
  prob <- case P.parse problem file of
    P.Fail _ _ err -> fail $ "Parse error: " ++ err
    P.Done "" p -> return p
    P.Done _ _ -> fail "Parse error: unhandled input"
  display (InWindow "visualize" (640, 480) (0, 0)) black (Scale 100 100 $ problemPicture prob)

main :: IO ()
main = getArgs >>= mapM_ visualize
