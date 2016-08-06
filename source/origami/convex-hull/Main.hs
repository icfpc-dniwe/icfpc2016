import ConvexHull
import Data.List
import Data.Ratio
import Linear.V2
import Types
import WolframAlpha
import System.Random

maxVertices :: Int
maxVertices = 50

outputFileName :: FilePath
outputFileName = "convex.wf"

formatVertices :: [IVertex] -> String
formatVertices vs = (show $ length vs) ++ "\n" ++ (intercalate "\n" $ map waShow vs)

instance (Random a) => Random (V2 a) where
  random g = let
    (x, g') = random g
    (y, g'') = random g'
    in (V2 x y, g'')
    
  randomR (V2 x1 y1, V2 x2 y2) g = let
    (x, g') = randomR (x1, x2) g
    (y, g'') = randomR (y1, y2) g'
    in (V2 x y, g'')


instance (Num a, Ord a, Integral a, Random a) => Random (Ratio a) where
  random g = let
    (n, g') = random g
    (d, g'') = random g'
    in (n % (if d == 0 then (fromInteger 1) else d), g'')

  randomR (l, r) g = let
    scale = 100
    cd = ((denominator l) * (denominator r)) * scale
    cl = (numerator l) * scale * (denominator r)
    cr = (numerator r) * scale * (denominator l)
    (n, g') = randomR (cl, cr) g
    in (n % cd, g')


main :: IO ()
main = getVertices maxVertices >>= outputVertices outputFileName where

  outputVertices :: FilePath -> [IVertex] -> IO ()
  outputVertices filepath vs = putStrLn -- writeFile
    -- filepath
    ((formatVertices vs) ++ "\n" ++ (formatVertices $ convexHull vs)) 

  getVertices :: Int -> IO [IVertex]
  getVertices n = (take n) . (randomRs (V2 0 0, V2 1 1)) <$> getStdGen

    

