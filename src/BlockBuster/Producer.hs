module BlockBuster.Producer
  ( DomainEvents (..),
    SlotInfo (..),
    DomainEvent (..),
    runProducer,
  )
where

import BlockBuster.ChainSync (ChainEvent, PollingClient)
import BlockBuster.ChainSync qualified as ChainSync
import Control.Monad (forever)
import Data.Void (Void)

data DomainEvents = DomainEvents [DomainEvent] SlotInfo
  deriving stock (Show)

data DomainEvent = DomainEvent
  deriving stock (Show)

data SlotInfo = SlotInfo
  deriving stock (Show)

-- TODO: something useful to provide access to the stream of domain events
type Producer = Void

runProducer :: (Show d) => PollingClient -> (ChainEvent -> d) -> IO Producer
runProducer pollClient cb = do
  forever $ do
    chainEvent <- ChainSync.poll pollClient
    let domainEvents = cb chainEvent
    putStrLn $ "Domain events: " <> show domainEvents
