{-# LANGUAGE OverloadedStrings #-}

module Types where

import Prelude hiding (takeWhile, Rational)
import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Char (ord)
import Data.List (intersperse)
import Data.ByteString (ByteString)
import Debug.Trace

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8


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


class WolframAlphaShow a where
  waShow :: a -> String

instance WolframAlphaShow Rational where
  waShow (Rational n d) = show $ (((fromIntegral n) / (fromIntegral d)) :: Double)

instance WolframAlphaShow Vertex where
  waShow (Vertex x y) = (waShow x) ++ " " ++ (waShow y)

instance WolframAlphaShow Polygon where
  waShow (Polygon vs) = concat $ intersperse "\n" $ (show (length vs)):(map waShow vs)

instance WolframAlphaShow Silhouette where
  waShow (Silhouette ps) = concat $ intersperse "\n" $ (show (length ps)):(map waShow ps)

instance WolframAlphaShow Segment where
  waShow (Segment a b) = (waShow a) ++ " " ++ (waShow b)

instance WolframAlphaShow Skeleton where
  waShow (Skeleton ss) = concat $ intersperse "\n" $ (show (length ss)):(map waShow ss)

instance WolframAlphaShow Problem where
  waShow (Problem silhouette skeleton) = (waShow silhouette) ++ "\n" ++ (waShow skeleton)


trace' x = trace (show x) x

read' = read . BSC8.unpack 
nl = string "\n"
spaces = takeWhile (inClass " ")
 
 
parseProblem :: ByteString -> Problem
parseProblem s = either (error "parsing error") id (parseOnly parser s) where
  parser = do
    silhouette <- parseSilhouette
    _          <- spaces *> nl
    skeleton   <- parseSkeleton
    return $ Problem silhouette skeleton

parseSilhouette :: Parser Silhouette
parseSilhouette = do
  n <- spaces *> parseNumber
  polygons <- sequence $ replicate n (spaces *> nl *> parsePolygon)
  return $ Silhouette polygons
  
parseSkeleton :: Parser Skeleton
parseSkeleton = do
  n <- spaces *> parseNumber
  segments <- sequence $ replicate n (spaces *> nl *> parseSegment)
  return $ Skeleton segments

parseSegment :: Parser Segment
parseSegment = do
  a <- parseVertex
  _ <- spaces
  b <- parseVertex
  return $ Segment a b

parseNumber :: Parser Int
parseNumber = do
  number <- takeWhile1 (\c -> let c' = (fromIntegral c) in ((ord '0') <= c') && (c' <= (ord '9')))
  return $ (read' number)

parseVertex :: Parser Vertex
parseVertex = do
  x <- spaces *> parseRational
  _ <- spaces *> (string ",")
  y <- spaces *> parseRational
  return $ Vertex x y

parseRational :: Parser Rational
parseRational = do
  numerator <- parseNumber
  denumerator <- (spaces *> (string "/") *> spaces *> parseNumber) <|> (spaces *> (return 1))
  return $ Rational numerator denumerator
  
parsePolygon :: Parser Polygon
parsePolygon = do
  n <- spaces *> parseNumber
  vertices <- sequence $ replicate n (spaces *> nl *> parseVertex)
  return $ Polygon vertices


