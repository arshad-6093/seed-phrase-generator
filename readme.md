Seed Phrase Generator
=====================

This Haskell program generates a 12-word seed phrase according to the BIP-39 standard. The seed phrase can be used for cryptographic purposes such as creating wallets for cryptocurrencies.

How It Works
------------
1. **Entropy Generation**: The program generates 128 bits of entropy.
2. **Checksum Calculation**: A checksum is created by hashing the entropy with SHA-256 and taking the first 4 bits.
3. **Binary Conversion**: The entropy and checksum are converted into a binary string.
4. **Word Mapping**: The binary string is split into 11-bit segments, each segment representing an index in the BIP-39 word list.
5. **Wordlist**: These indices are mapped to words in the BIP-39 wordlist to create the seed phrase.

Probability of Collision
------------------------
The probability of generating the same seed phrase twice is extremely low. Here’s why:

1. **Entropy**: Each seed phrase is based on 128 bits of entropy.
2. **Checksum**: An additional 4 bits are used for the checksum, making a total of 132 bits.

The total number of possible combinations for a 132-bit number is 2^132.

Calculation of Collision Probability
------------------------------------
For n = 2^132:

n = 2^132 ≈ 5.44 × 10^39

The probability P of generating the same seed phrase twice (collision) can be approximated as:

P ≈ 1 / 2n

So for a single pair:

P ≈ 1 / (2 × 5.44 × 10^39) ≈ 9.2 × 10^-40

Even for generating 10^9 (one billion) seed phrases, the collision probability is:

P ≈ (10^9)^2 / (2 × 2^132) = 10^18 / (2 × 5.44 × 10^39) ≈ 9.2 × 10^-22

Conclusion
----------
The chance of generating the same seed phrase twice is astronomically small. Therefore, each seed phrase is effectively unique in practical scenarios.

Usage
-----
1. **Load the BIP-39 Wordlist**: Ensure you have a `bip39.txt` file with the BIP-39 wordlist, each word on a new line.
2. **Run the Program**: Execute the Haskell script to generate a seed phrase.

```haskell
main :: IO ()
main = do
    -- Load the full BIP-39 wordlist
    wordlist <- loadWordlist "bip39.txt"
    seedPhrase <- generateSeedPhrase wordlist
    putStrLn "Generated Seed Phrase:"
    putStrLn $ unwords seedPhrase