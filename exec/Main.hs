module Main where

import           Data.Blockchain
import           Network.ToyBitcoin

import           GHC.Word           (Word32)

import           System.Random


randomBlockchainR :: (Hashable a, Random a) => (a, a) -> Word32 -> Blockchain a -> IO (Blockchain a)
randomBlockchainR _ 0 bc = return bc
randomBlockchainR r n bc@(Genesis block) = do
    b <- mineBlock initialTarget (blockHash block) <$> randomRIO r
    randomBlockchainR r (n - 1) (insertBlock b bc)
randomBlockchainR r n bc@(Node [x] _) = do
    b <- mineBlock initialTarget (blockHash x) <$> randomRIO r
    randomBlockchainR r (n - 1) (insertBlock b bc)
randomBlockchainR _ _ _ = error "randomBlockchainR: Non-linear blockchain"


randomBlockchain :: (Hashable a, Bounded a, Random a) => Word32 -> Blockchain a -> IO (Blockchain a)
randomBlockchain = randomBlockchainR (minBound, maxBound)


randomWord32BlockChain :: Word32 -> IO (Blockchain Word32)
randomWord32BlockChain n = randomBlockchainR (0,1000000) (n - 1) initialChain


main :: IO ()
main =
    {-
    bc <- randomWord32BlockChain 10
    putStrLn $ unlines $ reverse $ map show (toList bc)

    print (valid (const True) bc)
    -}

    startCompleteNetwork 10
