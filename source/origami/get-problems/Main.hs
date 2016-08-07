import Control.Monad
import Control.Concurrent
import qualified Data.Set as S
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import System.Directory
import Network.HTTP.Client
import Servant.Client

import API

delay :: MonadIO m => m ()
delay = liftIO $ threadDelay 1000000

query :: Manager -> ClientM ()
query manager = do
  snapHash <- snapshotHash <$> last <$> snapshots <$> listSnapshots manager icfpcUrl
  liftIO $ putStrLn $ "Latest snapshot: " ++ show snapHash
  delay
  snap <- getSnapshot snapHash manager icfpcUrl
  files <- liftIO $ S.fromList <$> getDirectoryContents "problems"
  forM_ (problems snap) $ \prob -> do
    let hash = problemSpecHash prob
    let name = show $ problemId prob
    unless (name `S.member` files) $ do
      liftIO $ putStrLn $ "Fetching " ++ name
      delay
      probText <- getProblem hash manager icfpcUrl
      liftIO $ B.writeFile ("snapshots/" ++ name) probText

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  res <- runExceptT $ query manager
  either (fail . show) return res
