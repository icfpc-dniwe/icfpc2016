module Main where

import Prelude hiding (readFile)
import Data.ByteString (readFile)
import System.Environment

import Types
import Parser

convert :: String -> IO ()
convert filename = (waShow . parseProblem) <$> (readFile filename)
  >>= writeFile (filename ++ ".wf")

main :: IO ()
main = getArgs >>= mapM_ convert 
