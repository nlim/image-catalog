module Main where

import qualified Server (main)

main :: IO ()
main = do
  putStrLn "Starting the Image Catalog Server using Haskell Servant"
  Server.main
