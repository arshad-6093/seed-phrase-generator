module Main where

import SeedPhrase (generateSeedPhrase)

main :: IO ()
main = do
  seedPhrase <- generateSeedPhrase
  putStrLn "Generated Seed Phrase:"
  putStrLn $ unwords seedPhrase