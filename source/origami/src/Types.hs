{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Types where

import Data.Ratio
import Linear.V2
import Linear.Epsilon
import System.Random


type VR = V2 Rational

instance Epsilon Rational where
  nearZero q = nearZero ((fromRational q) :: Double)
  
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

data Polygon = Polygon [VR]
  deriving (Eq, Show)

data Silhouette = Silhouette [Polygon]
  deriving (Eq, Show)

data Segment = Segment VR VR
  deriving (Eq, Show)

data Skeleton = Skeleton [Segment]
  deriving (Eq, Show)

data Problem = Problem Silhouette Skeleton
  deriving (Eq, Show)

data Solution
