module BlockBuster.ChainSync (
  startFollower,
  startClient,
  BBNodeInfo,
  mkConfig,
) where

import Cardano.Api qualified as CApi
import Cardano.Api.ChainSync.Client qualified as CApi.Sync
import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Control.Monad.IO.Class (MonadIO)

startFollower :: IO ()
startFollower = undefined

type Client c = c CApi.BlockInMode CApi.ChainPoint CApi.ChainTip IO ()

type BBNodeInfo = CApi.LocalNodeConnectInfo
type IntersectionPoint = CApi.ChainPoint

mkConfig socket netMagic =
  CApi.LocalNodeConnectInfo
    { localConsensusModeParams = CApi.CardanoModeParams mainnetEpochSlots
    , localNodeNetworkId = CApi.Testnet $ CApi.NetworkMagic netMagic
    , localNodeSocketPath = CApi.File socket
    }

toApiNodeInfo :: BBNodeInfo -> CApi.LocalNodeConnectInfo
toApiNodeInfo = id

startClient :: (MonadIO m) => BBNodeInfo -> [IntersectionPoint] -> m ()
startClient nodeInfo points =
  CApi.connectToLocalNode
    (toApiNodeInfo nodeInfo)
    CApi.LocalNodeClientProtocols
      { localChainSyncClient = CApi.LocalChainSyncClient $ chainSyncClient points
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

chainSyncClient points = CApi.ChainSyncClient $ pure (findIntersection points)
  where
    findIntersection points =
      CApi.Sync.SendMsgFindIntersect points $
        CApi.Sync.ClientStIntersect
          { CApi.Sync.recvMsgIntersectFound = \point tip -> CApi.ChainSyncClient $ do
              putStrLn $ "Intersection found: " -- <> show (point, tip)
              pure requestNext
          , CApi.Sync.recvMsgIntersectNotFound = \tip -> do
              CApi.ChainSyncClient $ do
                putStrLn $ "Intersection NOT found: " -- <> show tip
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
            putStrLn $ "Got roll forward: " -- <> show (block, tip)
            pure requestNext
        , CApi.Sync.recvMsgRollBackward = \point tip -> CApi.ChainSyncClient $ do
            putStrLn $ "Got rollback: " -- <> show (point, tip)
            pure requestNext
        }
