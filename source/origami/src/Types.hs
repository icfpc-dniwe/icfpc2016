module Types (module Types) where

import Prelude hiding (takeWhile, Rational)

data Rational = Rational Int Int
  deriving Show

data Vertex = Vertex Rational Rational
  deriving Show

data Polygon = Polygon [Vertex]
  deriving Show

data Silhouette = Silhouette [Polygon]
  deriving Show

data Segment = Segment Vertex Vertex
  deriving Show

data Skeleton = Skeleton [Segment]
  deriving Show

data Problem = Problem Silhouette Skeleton
  deriving Show

