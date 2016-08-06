module Types where

import Data.Ratio
import Linear.V2

type IVertex = V2 IRational
type IRational = Ratio Int

data Polygon = Polygon [IVertex]
  deriving Show

data Silhouette = Silhouette [Polygon]
  deriving Show

data Segment = Segment IVertex IVertex
  deriving Show

data Skeleton = Skeleton [Segment]
  deriving Show

data Problem = Problem Silhouette Skeleton
  deriving Show

