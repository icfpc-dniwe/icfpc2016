module Main where

import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString.Lazy as P

import Parser
import WolframAlpha

convert :: String -> IO ()
convert filename = do
  file <- BL.readFile filename
  prob <- case P.parse problem file of
    P.Fail _ _ err -> fail $ "Parse error: " ++ err
    P.Done "" p -> return p
    P.Done _ _ -> fail "Parse error: unhandled input"
  writeFile (filename ++ ".wf") (waShow prob)

main :: IO ()
main = getArgs >>= mapM_ convert 
