module Main where

import           Data.Blockchain

import           GHC.Word        (Word32)

import           System.Random


randomBlockchainR :: Random a => (a, a) -> Word32 -> Blockchain a -> IO (Blockchain a)
randomBlockchainR _ 0 bc = return bc
randomBlockchainR r n [] = do
    gb <- mineBlock initialTarget zeroHash <$> randomRIO r
    randomBlockchainR r (n - 1) [gb]
randomBlockchainR r n (x:bc) = do
    b <- mineBlock initialTarget (blockHash x) <$> randomRIO r
    randomBlockchainR r (n - 1) (b:x:bc)


randomBlockchain :: (Bounded a, Random a) => Word32 -> Blockchain a -> IO (Blockchain a)
randomBlockchain = randomBlockchainR (minBound, maxBound)


randomWord32BlockChain :: Word32 -> IO (Blockchain Word32)
randomWord32BlockChain n = randomBlockchainR (0,1000000) (n - 1) [genesisBlock]


main :: IO ()
main = do
    bc <- randomWord32BlockChain 10
    putStrLn $ unlines $ reverse $ map show bc

    print (valid (const True) bc)
