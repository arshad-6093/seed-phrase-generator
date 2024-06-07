module SeedPhrase where

import           Control.Monad (replicateM)
import qualified Crypto.Hash as Crypto
import           Data.ByteString ()
import qualified Data.ByteString.Char8 as BS
import           Data.ByteArray (convert)
import           Data.Bits (shiftR)
import           Data.Byteable ()
import           Data.Char (digitToInt, intToDigit)
import           Data.List (foldl', unfoldr)
import           System.Random (randomRIO)
import           Numeric (showIntAtBase)

-- Load the BIP-39 wordlist from a file
loadWordlist :: FilePath -> IO [String]
loadWordlist path = do
    content <- readFile path
    return $ lines content

-- Generate a random seed phrase of 12 words
generateSeedPhrase :: IO [String]
generateSeedPhrase = do
  wordlist <- loadWordlist "src/bip39.txt"
  -- Generate 128 bits of entropy (16 bytes)
  entropy <- replicateM 16 (randomRIO (0, 255) :: IO Int)
  let entropyBytes = BS.pack $ map (toEnum . fromEnum) entropy

  -- Hash the entropy with SHA-256 to get the checksum
  let hash = Crypto.hashWith Crypto.SHA256 entropyBytes
      hashBytes = convert hash  -- Convert digest to ByteString
      checksumBits = take 1 (BS.unpack hashBytes)  -- 4 bits for 128 bits of entropy
      checksum = fromEnum (head checksumBits) `shiftR` 4  -- Extract the first 4 bits

  -- Convert entropy + checksum to a binary string
  let binaryString = concatMap (padLeft 8 '0' . flip showBinary 2) entropy
      binaryWithChecksum = binaryString ++ padLeft 4 '0' (showBinary checksum 2)

  -- Split the binary string into 11-bit chunks and map to words
  let wordIndices = unfoldr (\b -> if null b then Nothing else Just $ splitAt 11 b) binaryWithChecksum
      wordIndicesInt = map binaryToInt wordIndices
  return $ map (wordlist !!) wordIndicesInt

-- Helper functions
padLeft :: Int -> a -> [a] -> [a]
padLeft n c xs = replicate (n - length xs) c ++ xs

showBinary :: Int -> Int -> String
showBinary x n = padLeft n '0' $ showIntAtBase 2 intToDigit x ""

binaryToInt :: String -> Int
binaryToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0