module Types where

import Linear.V2

type VR = V2 Rational

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
