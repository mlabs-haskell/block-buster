module BlockBuster.ChainSync
  ( startClient,
    ConnectionConfig,
    mkConfig,
    ChainEvent (..),
    spawnPollClient,
    poll,
    stopSyncing,
    PollingClient,
  )
where

import Cardano.Api qualified as CApi
import Cardano.Api.ChainSync.Client qualified as CApi.Sync
import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Chan.Unagi.Bounded qualified as UC
import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry (RetryPolicyM, retrying, rsPreviousDelay)
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

data PollingClient
  = PollChainSyncClient
      (Async.Async ())
      (UC.OutChan ChainEvent)

data ChainEvent
  = RollForward CApi.BlockInMode CApi.ChainTip
  | RollBack CApi.ChainPoint CApi.ChainTip
  deriving stock (Show)

type ConnectionConfig = CApi.LocalNodeConnectInfo

type IntersectionPoint = CApi.ChainPoint

spawnPollClient :: ConnectionConfig -> RetryPolicyM IO -> [IntersectionPoint] -> IO PollingClient
spawnPollClient nodeInfo retryPolicy points = do
  (inChan, outCahn) <- UC.newChan 4
  let callback ev = do
        putStrLn "Writing message to chan"
        UC.writeChan inChan ev
  res <- Async.async $ do
    startClient nodeInfo retryPolicy points callback
  pure $ PollChainSyncClient res outCahn

poll :: PollingClient -> IO ChainEvent
poll (PollChainSyncClient _ outChan) = UC.readChan outChan

stopSyncing :: PollingClient -> IO ()
stopSyncing (PollChainSyncClient as _) = Async.cancel as

mkConfig :: FilePath -> Word32 -> ConnectionConfig
mkConfig socket netMagic =
  CApi.LocalNodeConnectInfo
    { localConsensusModeParams = CApi.CardanoModeParams mainnetEpochSlots,
      localNodeNetworkId = CApi.Testnet $ CApi.NetworkMagic netMagic,
      localNodeSocketPath = CApi.File socket
    }

toApiNodeInfo :: ConnectionConfig -> CApi.LocalNodeConnectInfo
toApiNodeInfo = id

-- Cardano Api chain sync client code
type ChainSyncCallback = ChainEvent -> IO ()

startClient ::
  ConnectionConfig ->
  RetryPolicyM IO ->
  [IntersectionPoint] ->
  ChainSyncCallback ->
  IO ()
startClient nodeInfo retryPolicy points callback = do
  let connectionPolicy =
        CApi.connectToLocalNode
          (toApiNodeInfo nodeInfo)
          CApi.LocalNodeClientProtocols
            { localChainSyncClient = CApi.LocalChainSyncClient $ chainSyncClient points callback,
              localTxSubmissionClient = Nothing,
              localStateQueryClient = Nothing,
              localTxMonitoringClient = Nothing
            }
  maybeResult <-
    retrying
      retryPolicy
      ( \retryStatus y -> do
          let isRetrying = isLeft y
          if isRetrying
            then
              putStrLn $ "Delayed " ++ show (fromMaybe 0 $ rsPreviousDelay retryStatus)
            else
              putStrLn "Giving up"
          pure isRetrying
      )
      $ const
        ( liftIO $
            try connectionPolicy ::
            (MonadIO m) => m (Either IOException ())
        )
  case maybeResult of
    Right result -> pure result
    Left err -> do
      putStrLn "Failed to establish initial node connection"
      error $ show err

type Client c = c CApi.BlockInMode CApi.ChainPoint CApi.ChainTip IO ()

chainSyncClient ::
  [CApi.ChainPoint] ->
  ChainSyncCallback ->
  Client CApi.Sync.ChainSyncClient
chainSyncClient intersectionPoints callback =
  CApi.ChainSyncClient $ pure (findIntersection intersectionPoints)
  where
    findIntersection points =
      CApi.Sync.SendMsgFindIntersect points $
        CApi.Sync.ClientStIntersect
          { CApi.Sync.recvMsgIntersectFound = \_point _tip -> CApi.ChainSyncClient $ do
              putStrLn "CAPiClient:  Intersection found"
              pure requestNext,
            CApi.Sync.recvMsgIntersectNotFound = \tip -> do
              CApi.ChainSyncClient $ do
                putStrLn "CAPiClient:  Intersection NOT found"
                if null points
                  then pure $ findIntersection [CApi.chainTipToChainPoint tip]
                  else do
                    pure $ CApi.Sync.SendMsgDone ()
          }

    requestNext :: Client CApi.Sync.ClientStIdle
    requestNext = CApi.Sync.SendMsgRequestNext (pure ()) handleNext

    handleNext =
      CApi.Sync.ClientStNext
        { CApi.Sync.recvMsgRollForward = \block tip -> CApi.ChainSyncClient $ do
            putStrLn "CAPiClient:  Got roll forward"
            callback $ RollForward block tip
            pure requestNext,
          CApi.Sync.recvMsgRollBackward = \point tip -> CApi.ChainSyncClient $ do
            putStrLn "CAPiClient: Got rollback"
            callback $ RollBack point tip
            pure requestNext
        }
