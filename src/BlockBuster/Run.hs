module BlockBuster.Run (run) where

import BlockBuster.ChainSync (startClient)
import Cardano.Api qualified as CApi
import Cardano.Chain.Epoch.File (mainnetEpochSlots)

run :: IO ()
run = do
  putStrLn "BlockBuster test!"
  let nodeInfo =
        CApi.LocalNodeConnectInfo
          { localConsensusModeParams = CApi.CardanoModeParams mainnetEpochSlots
          , localNodeNetworkId = CApi.Testnet $ CApi.NetworkMagic 42
          , localNodeSocketPath = CApi.File "local-node.sock"
          }
  startClient nodeInfo []
