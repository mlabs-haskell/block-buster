module BlockBuster.Run (run) where

import Cardano.Api qualified as CApi
import Cardano.Api.ChainSync.Client qualified as CApi.Sync

run :: IO ()
run = putStrLn "BlockBuster!"