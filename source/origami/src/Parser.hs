module Parser
       ( problem
       ) where

import Types

import Data.Ratio
import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Linear.V2

problem :: Parser Problem
problem = do
  sil <- silhouette
  skipSpace
  sk <- skeleton
  skipSpace
  return $ Problem sil sk

silhouette :: Parser Silhouette
silhouette = do
  n <- decimal
  polygons <- replicateM n (skipSpace *> polygon)
  return $ Silhouette polygons
  
skeleton :: Parser Skeleton
skeleton = do
  n <- decimal
  segments <- replicateM n (skipSpace *> segment)
  return $ Skeleton segments

segment :: (Integral a) => Parser (Segment (Ratio a))
segment = do
  a <- vertex
  skipSpace
  b <- vertex
  return $ Seg a b

vertex :: (Integral a) => Parser (V2 (Ratio a))
vertex = do
  x <- ratio
  _ <- skipSpace *> string ","
  y <- skipSpace *> ratio
  return $ V2 x y

ratio :: (Integral a) => Parser (Ratio a)
ratio = do
  n <- signed decimal
  skipSpace
  d <- (string "/" *> skipSpace *> decimal) <|> (return 1)
  return (n % d)

polygon :: (Integral a) => Parser (Polygon (Ratio a))
polygon = do
  n <- decimal
  vertices <- replicateM n (skipSpace *> vertex)
  return $ Polygon vertices
