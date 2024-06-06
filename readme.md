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

Let's break down the seed phrase generation process step by step, using simpler language and concrete examples.

Step 1: Generate Entropy
Entropy is just a fancy term for randomness. In this step, we generate a random sequence of 128 bits. Each bit can be either 0 or 1, so 128 bits give us a very large number of possible sequences.

Let's say we generate the following 128-bit entropy:

1100110101111001010110000101100101001001001011111011101011101100110101110101111101010000101001011000110111101110
Step 2: Calculate the Checksum
Next, we calculate a checksum. A checksum is a way to verify that data has not been corrupted. We hash our entropy using the SHA-256 algorithm, which produces a 256-bit result. However, we only need the first 4 bits of this result.

Suppose the SHA-256 hash of our entropy looks like this (256 bits):

0110110101111010010101000101100101001001001011111011101011101100110101110101111101010000101001011000110111101110111101110101101110010110001101101001010100011111
We take the first 4 bits:

yaml
0110
Step 3: Combine Entropy and Checksum
We append these 4 checksum bits to our original 128 bits of entropy, resulting in a total of 132 bits.

yaml
Entropy:  1100110101111001010110000101100101001001001011111011101011101100110101110101111101010000101001011000110111101110
Checksum: 0110
Combined: 11001101011110010101100001011001010010010010111110111010111011001101011101011111010100001010010110001101111011100110
Step 4: Convert to Binary String
We now have a 132-bit binary string:

11001101011110010101100001011001010010010010111110111010111011001101011101011111010100001010010110001101111011100110
Step 5: Split Into 11-bit Chunks
We split this 132-bit string into chunks of 11 bits each. This gives us 12 chunks because 132 / 11 = 12.

11001101011
11001010110
00010110010
10010010010
11110111010
11101100110
11010111010
11110101000
00101001010
00110111011
10011000011
0011 (last chunk, 11 bits padded to fit 12)
Step 6: Convert Each Chunk to a Decimal Number
We convert each 11-bit chunk to a decimal number. Here are the conversions for our example:


11001101011 -> 1643
11001010110 -> 1622
00010110010 -> 50
10010010010 -> 1170
11110111010 -> 2010
11101100110 -> 1894
11010111010 -> 1754
11110101000 -> 2008
00101001010 -> 330
00110111011 -> 443
10011000011 -> 1219
0011 (padded) -> 3

Step 7: Map to Word List
Finally, we map each of these decimal numbers to words in the BIP-39 word list. Suppose our word list is:

["abandon", "ability", "able", "about", ..., "zoo"]
We select words based on the indices we calculated:

1643 -> "exhibit"
1622 -> "excess"
50   -> "about"
1170 -> "drill"
2010 -> "year"
1894 -> "upset"
1754 -> "stadium"
2008 -> "wrist"
330  -> "clock"
443  -> "couch"
1219 -> "ethics"
3    -> "about" (repeats due to small example list)
So, our generated seed phrase is:

"exhibit excess about drill year upset stadium wrist clock couch ethics about"

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
