module API where

import GHC.Generics
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Servant.API
import Servant.Client
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Media as M

import Types
import Emit

newtype APIKey = APIKey Text
               deriving (Eq, Show, ToHttpApiData, IsString)

type IcfpcCommon = Header "X-API-Key" APIKey

type IcfpcAPI = Header "X-API-Key" APIKey :> IcfpcAPI'

icfpcParseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> JSON.Parser a
icfpcParseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'
                                                 }

data HelloResponse = HelloResponse { greeting :: Text }
                   deriving (Show, Eq, Generic)

instance FromJSON HelloResponse where
  parseJSON = icfpcParseJSON

type Hash = Text

newtype SnapshotHash = SnapshotHash { unSnapshotHash :: Text }
                     deriving (Eq, Show, FromJSON, ToHttpApiData)

data SnapshotEntry = SnapshotEntry { snapshotTime :: POSIXTime
                                   , snapshotHash :: SnapshotHash
                                   }
                deriving (Eq, Show, Generic)

instance FromJSON SnapshotEntry where
  parseJSON = icfpcParseJSON

data SnapshotsResponse = SnapshotsResponse { snapshots :: [SnapshotEntry] }
                  deriving (Eq, Show, Generic)

instance FromJSON SnapshotsResponse where
  parseJSON = icfpcParseJSON

newtype ProblemHash = ProblemHash { unProblemHash :: Text }
                    deriving (Eq, Show, FromJSON, ToHttpApiData)

data ProblemEntry = ProblemEntry { problemSpecHash :: ProblemHash
                                 , problemId :: Integer
                                 }
              deriving (Eq, Show, Generic)

instance FromJSON ProblemEntry where
  parseJSON = icfpcParseJSON

data Snapshot = Snapshot { problems :: [ProblemEntry]
                         }
              deriving (Eq, Show, Generic)

instance FromJSON Snapshot where
  parseJSON = icfpcParseJSON

-- data ProblemResponse = ProblemResponse { problemId :: Integer
--                                        , publishTime :: POSIXTime
--                                        , solutionSpecHash :: Hash
--                                        , solutionSize :: Integer
--                                        , problemSpecHash :: Hash
--                                        , problemSize :: Integer
--                                        }
--                        deriving (Show, Eq, Generic)

-- instance FromJSON ProblemResponse where
--   parseJSON = icfpcParseJSON

data ProblemEncoded

instance Accept ProblemEncoded where
  contentType _ = "text" M.// "plain"

-- instance MimeRender ProblemEncoded Problem where
--   mimeUnrender _ = emitProblem

instance MimeUnrender ProblemEncoded B.ByteString where
  mimeUnrender _ = Right . BL.toStrict

data SolutionResponse = SolutionResponse { resemblance :: Double
                                         , solutionSpecHash :: Hash
                                         , solutionSize :: Integer
                                         }
                       deriving (Show, Eq, Generic)

instance FromJSON SolutionResponse where
  parseJSON = icfpcParseJSON

data SolutionEncoded

instance Accept SolutionEncoded where
  contentType _ = contentType (Proxy :: Proxy PlainText)

instance MimeRender SolutionEncoded Solution where
  mimeRender _ = emitSolution

type IcfpcAPI' = "hello" :> Get '[JSON] HelloResponse
                :<|> "snapshot" :> "list" :> Get '[JSON] SnapshotsResponse
                :<|> "blob" :> Capture "hash" SnapshotHash :> Get '[JSON] Snapshot
                :<|> "blob" :> Capture "hash" ProblemHash :> Get '[ProblemEncoded] B.ByteString
                -- :<|> "problem" :> "submit" :> ReqBody '[ProblemEncoded] Problem :> Post '[JSON] ProblemResponse
                :<|> "solution" :> "submit" :> ReqBody '[SolutionEncoded] Solution :> Post '[JSON] SolutionResponse

hello :: Manager -> BaseUrl -> ClientM HelloResponse
listSnapshots :: Manager -> BaseUrl -> ClientM SnapshotsResponse
getSnapshot :: SnapshotHash -> Manager -> BaseUrl -> ClientM Snapshot
getProblem :: ProblemHash -> Manager -> BaseUrl -> ClientM B.ByteString
submitSolution :: Solution -> Manager -> BaseUrl -> ClientM SolutionResponse
(hello :<|> listSnapshots :<|> getSnapshot :<|> getProblem :<|> submitSolution) = client (Proxy :: Proxy IcfpcAPI) (Just "224-dc056a82e0e9b5443625dbf5e6e43e5e")

icfpcUrl :: BaseUrl
icfpcUrl = BaseUrl Http "2016sv.icfpcontest.org" 80 "/api"
