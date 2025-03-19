module Main (main) where

import BlockBuster.ChainSync
  ( spawnPollClient,
  )
import BlockBuster.Config (getDaemonConnectionInfo, getDaemonRetryPolicy, loadConfigFile)
import BlockBuster.Producer
  ( runProducer,
  )
import Data.Either (fromRight)

main :: IO ()
main = do
  daemonConfig <- fromRight (error "Config parsing failed") <$> loadConfigFile "daemon.toml"
  putStrLn "BlockBuster test!"
  print $ daemonConfig
  pollClient <- spawnPollClient (getDaemonConnectionInfo daemonConfig) (getDaemonRetryPolicy daemonConfig) []
  _ <- runProducer pollClient id
  -- forever $ do
  --   chainEvent <- poll pollClient
  --   putStrLn $ "Polled message: " <> show chainEvent
  pure ()
