module Main where

import Prelude hiding (readFile)
import Data.ByteString (readFile)
import System.Environment

import Types

main :: IO ()
main = getArgs >>= mapM_ convert where
  convert filename = (waShow . parseProblem) <$> (readFile filename)
    >>= writeFile (filename ++ ".new")
