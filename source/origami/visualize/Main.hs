import qualified Data.Set as S
import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString.Lazy as P
import Graphics.Gloss.Interface.Pure.Display hiding (Polygon, color)
import qualified Graphics.Gloss.Interface.Pure.Display as G
import Linear.V2

import Types
import Parser
import Math
import Convexize

pointV2 :: (Real a) => V2 a -> Point
pointV2 (fmap (fromRational . toRational) -> V2 x y) = (x, y)

polygonPicture :: (Real a) => Polygon a -> Picture
polygonPicture = Pictures . map triPicture . ketTri
  where triPicture (Tri a b c) = G.Polygon [pointV2 a, pointV2 b, pointV2 c]

silhouettePicture :: Silhouette -> Picture
silhouettePicture (Silhouette silh) = Pictures $ map polygonize silh
  where polygonize pol = Color color $ polygonPicture pol
          where color
                  | clockwise pol = white
                  | otherwise = red

segmentPicture :: (Real a) => (Segment a) -> Picture
segmentPicture (Seg a b) = Color color $ Line [pointV2 a, pointV2 b]
  where color = blue

skeletonPicture :: Skeleton -> Picture
skeletonPicture (Skeleton skel) = Pictures $ map segmentPicture skel

problemPicture :: Problem -> Picture
problemPicture (Problem silh skel) = Pictures [silhouettePicture silh, skeletonPicture skel]

convexPicture :: (Real a) => Convex a -> Picture
convexPicture (Convex conv) = G.Polygon $ map pointV2 $ S.toList conv

convSkeletonPicture :: ConvSkeleton -> Picture
convSkeletonPicture (ConvSkeleton cskel) = Color color $ Pictures $ map convexPicture $ S.toList cskel
  where color = green

allPicture :: Problem -> Picture
allPicture (Problem silh skel) = Pictures [silhouettePicture silh, convSkeletonPicture $ convexize skel]

visualize :: FilePath -> IO ()
visualize probPath = do
  file <- BL.readFile probPath
  prob <- case P.parse problem file of
    P.Fail _ _ err -> fail $ "Parse error: " ++ err
    P.Done "" p -> return p
    P.Done _ _ -> fail "Parse error: unhandled input"
  display (InWindow "visualize" (640, 480) (0, 0)) black (Scale 100 100 $ allPicture prob)

main :: IO ()
main = getArgs >>= mapM_ visualize
