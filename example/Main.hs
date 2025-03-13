module Main (main) where

import BlockBuster.ChainSync (
  ChainEvent (RollBack, RollForward),
  mkConfig,
  poll,
  spawnPollClient,
  startClient,
 )
import BlockBuster.Producer (
  DomainEvents (DomainEvents),
  SlotInfo (SlotInfo),
  runProducer,
 )
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = do
  putStrLn "BlockBuster test!"
  pollClient <- spawnPollClient (mkConfig "local-node.sock" 42) []
  let
    dummyNoEvents = DomainEvents [] SlotInfo
    domainEventsCb = const dummyNoEvents
  runProducer pollClient domainEventsCb
  --   forever $ do
  --       chainEvent <- poll pollClient
  --       putStrLn $ "Polled message: " <> show chainEvent
  pure ()
