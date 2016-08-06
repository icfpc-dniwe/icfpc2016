module Main where

import qualified Data.Text.Lazy.IO as TL
import qualified Data.Attoparsec.Text.Lazy as P
import System.Environment

import Parser
import WolframAlpha

convert :: String -> IO ()
convert filename = do
  file <- TL.readFile filename
  prob <- case P.parse problem file of
    P.Fail _ _ err -> fail $ "Parse error: " ++ err
    P.Done "" p -> return p
    P.Done _ _ -> fail "Parse error: unhandled input"
  writeFile (filename ++ ".wf") (waShow prob)

main :: IO ()
main = getArgs >>= mapM_ convert 
