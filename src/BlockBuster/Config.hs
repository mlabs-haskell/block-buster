{-# LANGUAGE NamedFieldPuns #-}

module BlockBuster.Config where

import Cardano.Api qualified as CApi
import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Control.Retry (RetryPolicyM, RetryStatus (..), capDelay, limitRetries, retryPolicy)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Toml (TomlCodec, (.=))
import Toml qualified

data NodeConnectionType = N2C
  deriving stock (Show)

data NodeConfig = NodeConfig
  { type' :: NodeConnectionType,
    socketPath :: FilePath,
    networkMagic :: Word32
  }
  deriving stock (Show)

nodeCodec :: TomlCodec NodeConfig
nodeCodec =
  Toml.table
    ( NodeConfig . const N2C
        <$> Toml.text "type" .= const "n2c"
        <*> Toml.string "socket_path" .= socketPath
        <*> (fromIntegral . fromMaybe 42 <$> Toml.dioptional (Toml.integer "network_magic")) .= Just . fromIntegral . networkMagic
    )
    "source"

data IntersectionConfig = FromTip | FromGenesis | FromIntersection CApi.SlotNo (CApi.Hash CApi.BlockHeader)
  deriving stock (Show)

intersectionCodec :: TomlCodec IntersectionConfig
intersectionCodec =
  Toml.table
    ( Toml.dimap
        ( \case
            FromTip -> ("tip", Nothing)
            FromGenesis -> ("genesis", Nothing)
            FromIntersection slot hash -> ("point", Just (slot, hash))
        )
        ( \case
            ("tip", Nothing) -> FromTip
            ("genesis", Nothing) -> FromGenesis
            ("point", Just (slot, hash)) -> FromIntersection slot hash
            ("tip", _) -> error "Intersection type tip may have no other keys"
            ("genesis", _) -> error "Intersection type genesis may have no other keys"
            ("point", Nothing) -> error "Intersection type point must have the point key"
            _ -> error "Unknown intersection type"
        )
        parser
    )
    "intersect"
  where
    parser :: TomlCodec (Text, Maybe (CApi.SlotNo, CApi.Hash CApi.BlockHeader))
    parser =
      (,) . T.toLower
        <$> Toml.text "type" .= fst
        <*> Toml.dioptional pointCodec .= snd
    pointCodec :: TomlCodec (CApi.SlotNo, CApi.Hash CApi.BlockHeader)
    pointCodec =
      Toml.table
        ( (,)
            . CApi.SlotNo
            . fromIntegral
            <$> Toml.integer "slot" .= (fromIntegral . CApi.unSlotNo . fst)
            <*> ( either
                    (error . show)
                    id
                    . CApi.deserialiseFromRawBytesHex
                      ( CApi.proxyToAsType (Proxy :: Proxy (CApi.Hash CApi.BlockHeader))
                      )
                    <$> Toml.byteString "hash"
                )
              .= (CApi.serialiseToRawBytesHex . snd)
        )
        "point"

data RetriesConfig = RetriesConfig
  { maxRetries :: Integer,
    backoffUnitSec :: Integer,
    backoffFactor :: Integer,
    maxBackoffSec :: Integer
  }
  deriving stock (Show)

defaultRetriesConfig :: RetriesConfig
defaultRetriesConfig =
  RetriesConfig
    { maxRetries = 3,
      backoffUnitSec = 1,
      backoffFactor = 3,
      maxBackoffSec = 10
    }

retriesCodec :: TomlCodec RetriesConfig
retriesCodec =
  Toml.table
    ( RetriesConfig
        <$> usingDefault "max_retries" maxRetries
        <*> usingDefault "backoff_unit_sec" backoffUnitSec
        <*> usingDefault "backoff_factor" backoffFactor
        <*> usingDefault "max_backoff_sec" maxBackoffSec
    )
    "retries"
  where
    usingDefault key accessor = (fromMaybe (accessor defaultRetriesConfig) <$> Toml.dioptional (Toml.integer key)) .= Just . accessor

data DaemonConfig = DaemonConfig
  { source :: NodeConfig,
    intersection :: IntersectionConfig,
    retries :: RetriesConfig
  }
  deriving stock (Show)

daemonCodec :: TomlCodec DaemonConfig
daemonCodec =
  DaemonConfig
    <$> nodeCodec .= source
    <*> intersectionCodec .= intersection
    <*> (fromMaybe defaultRetriesConfig <$> Toml.dioptional retriesCodec) .= Just . retries

loadConfigFile :: FilePath -> IO (Either [Toml.TomlDecodeError] DaemonConfig)
loadConfigFile = Toml.decodeFileEither daemonCodec

-- when supported, type' should indicate either N2C or N2N connections
getConnectionInfo :: NodeConfig -> CApi.LocalNodeConnectInfo
getConnectionInfo NodeConfig {type', socketPath, networkMagic} =
  case type' of
    N2C ->
      CApi.LocalNodeConnectInfo
        { localConsensusModeParams = CApi.CardanoModeParams mainnetEpochSlots,
          localNodeNetworkId = if networkMagic == 764824073 then CApi.Mainnet else CApi.Testnet $ CApi.NetworkMagic networkMagic,
          localNodeSocketPath = CApi.File socketPath
        }

getDaemonConnectionInfo :: DaemonConfig -> CApi.LocalNodeConnectInfo
getDaemonConnectionInfo = getConnectionInfo . source

getRetryPolicy :: (Monad m) => RetriesConfig -> RetryPolicyM m
getRetryPolicy
  RetriesConfig
    { maxRetries,
      backoffUnitSec,
      backoffFactor,
      maxBackoffSec
    } =
    capDelay
      (secondsToMicroseconds . fromIntegral $ maxBackoffSec)
      (backoffPolicy <> (limitRetries . fromIntegral $ maxRetries))
    where
      secondsToMicroseconds :: Int -> Int
      secondsToMicroseconds = (* (10 ^ 6))
      backoffPolicy :: (Monad m) => RetryPolicyM m
      backoffPolicy = retryPolicy $
        \RetryStatus {rsIterNumber} -> Just $! backoffFn (rsIterNumber + 1)

      backoffFn :: Int -> Int
      backoffFn x = secondsToMicroseconds ((x ^ backoffFactor) * fromIntegral backoffUnitSec)

getDaemonRetryPolicy :: (Monad m) => DaemonConfig -> RetryPolicyM m
getDaemonRetryPolicy = getRetryPolicy . retries
