module Main (main) where

import BlockBuster.ChainSync (startClient, mkConfig)

main :: IO ()
main = do
  putStrLn "BlockBuster test!"
  startClient (mkConfig "local-node.sock" 42) []