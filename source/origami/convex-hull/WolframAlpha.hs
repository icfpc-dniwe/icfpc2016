module WolframAlpha where

import Data.List
import Data.Ratio
import Linear.V2

import Types

class WolframAlphaShow a where
  waShow :: a -> String

instance WolframAlphaShow Int where
  waShow = show

instance WolframAlphaShow Double where
  waShow = show

instance Integral a => WolframAlphaShow (Ratio a) where
  waShow n = waShow (fromIntegral (numerator n) / fromIntegral (denominator n) :: Double)

instance WolframAlphaShow a => WolframAlphaShow (V2 a) where
  waShow (V2 x y) = waShow x ++ " " ++ waShow y

instance WolframAlphaShow Polygon where
  waShow (Polygon vs) = concat $ intersperse "\n" (show (length vs) : map waShow vs)

instance WolframAlphaShow Silhouette where
  waShow (Silhouette ps) = concat $ intersperse "\n" $ (show (length ps) : map waShow ps)

instance WolframAlphaShow Segment where
  waShow (Segment a b) = waShow a ++ " " ++ waShow b

instance WolframAlphaShow Skeleton where
  waShow (Skeleton ss) = concat $ intersperse "\n" $ (show (length ss) : map waShow ss)

instance WolframAlphaShow Problem where
  waShow (Problem silhouette skeleton) = waShow silhouette ++ "\n" ++ waShow skeleton

