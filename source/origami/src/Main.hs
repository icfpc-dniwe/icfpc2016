module Main where

import Prelude hiding (readFile)
import Data.ByteString (readFile)

import Types

main :: IO ()
main = (waShow . parseProblem) <$> (readFile "examples/silhouette.txt") >>= putStrLn
