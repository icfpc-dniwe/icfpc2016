import ConvexHull
import PaperFolding
import Math       
import Data.List
import Types
import Linear.V2
import System.Random


maxVertices :: Int
maxVertices = 10

fmtVR :: VR -> String
fmtVR (V2 x y) = "(" ++ (show' x) ++ ", " ++ (show' y) ++ ")" where
  show' z = show ((fromRational z) :: Double)


fmtSegment :: Segment -> String
fmtSegment (Segment a b) = "[" ++ (fmtVR a) ++ "; " ++ (fmtVR b) ++ "]" 


(<$$>) f mmx = (f <$>) <$> mmx
  

test1 = foldl1 (.) [
    (+ 0.5)
  , (* (1/4))
  ] <$$> [
    V2 0 (-1)
  , V2 1 0
  , V2 0 1
  , V2 (-1) 0 
  ]

  
main :: IO ()
main = do
  let (Paper (Wireframe ws) (Mapping ms)) = wrapConvexHull test1
  -- putStrLn $  intercalate "\n" (map fmtVR test1)

  putStrLn $ intercalate "\n" $ map fmtSegment ws
  putStrLn $ "-----"
  putStrLn $ intercalate "\n" $ map (\(a, a') -> (fmtVR a) ++ " -> " ++ (fmtVR a')) ms
  

--  getVertices maxVertices >>= outputVertices where
--
--  outputVertices :: [VR] -> IO ()
--  outputVertices vs = putStrLn "TODO"
--
--  getVertices :: Int -> IO [VR]
--  getVertices n = convexHull . (take n) . (randomRs (V2 0 0, V2 1 1)) <$> getStdGen

    

