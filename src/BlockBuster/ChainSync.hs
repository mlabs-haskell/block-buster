module BlockBuster.ChainSync (
  startFollower,
  BBNodeInfo,
) where

import Cardano.Api qualified as CApi
import Cardano.Api.ChainSync.Client qualified as CApi.Sync
import Control.Monad.IO.Class (MonadIO)

startFollower :: IO ()
startFollower = undefined

data BBNodeInfo

toApiNodeInfo :: BBNodeInfo -> CApi.LocalNodeConnectInfo
toApiNodeInfo = undefined

startClient :: (MonadIO m) => BBNodeInfo -> m ()
startClient nodeInfo =
  CApi.connectToLocalNode
    (toApiNodeInfo nodeInfo)
    CApi.LocalNodeClientProtocols
      { localChainSyncClient = CApi.LocalChainSyncClient chainSyncClient
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

chainSyncClient = undefined
