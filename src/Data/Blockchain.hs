{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Blockchain where

import qualified Crypto.Hash.SHA256     as SHA256

import           Data.Binary
import           Data.Binary.Put        (putByteString, putWord32le, runPut)
import           Data.ByteString
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import           Data.ByteString.Lazy   (toStrict)
import           Data.List              as L


class Hashable a where
    putHash :: a -> Put


instance Hashable ByteString where
    putHash = put


newtype Hash256 = Hash256 ByteString
    deriving (Eq, Ord, Binary)


instance Hashable Hash256 where
    putHash (Hash256 bs) = putByteString bs


instance Show Hash256 where
    show (Hash256 bs) = C8.unpack (B16.encode bs)


initialTarget :: Hash256
initialTarget =
    Hash256 (pack (Prelude.take 8 (allZeros ++ allOnes)))
  where
    allOnes = Prelude.repeat maxBound
    allZeros = Prelude.replicate 2 minBound


zeroHash :: Hash256
zeroHash = Hash256 (pack (Prelude.replicate 8 minBound))


sha256 :: Hashable a => a -> Hash256
sha256 = Hash256 . SHA256.hash . toStrict . runPut . putHash


data Block a = Block
    { blockHeader :: BlockHeader
    , body        :: a
    } deriving (Show, Eq)


blockHash :: Block a -> Hash256
blockHash = sha256 . blockHeader


data BlockHeader = BlockHeader
    { prevHash :: Hash256
    , nonce    :: Word32
    } deriving (Show, Eq)


instance Hashable BlockHeader where
    putHash (BlockHeader p n) =
        put p >> putWord32le n


type Blockchain a = [Block a]


valid :: (Block a -> Bool) -> Blockchain a -> Bool
valid p [x] = p x && blockSolution initialTarget (blockHeader x)
valid p (x:y:bs)
    | p x && blockSolution initialTarget (blockHeader x) && prevHash (blockHeader x) == blockHash y =
        valid p (y:bs)
valid _ _ = False


mineBlock :: Hash256 -> Hash256 -> a -> Block a
mineBlock target parentHash = Block minedHeader
  where
    nonces = [0..]
    headers = L.map (BlockHeader parentHash) nonces
    minedHeader = L.head (L.filter (blockSolution target) headers)


blockSolution :: Hash256 -> BlockHeader -> Bool
blockSolution target trialHeader =
    sha256 (sha256 trialHeader) < target


genesisBlock :: Block Word32
genesisBlock = mineBlock initialTarget zeroHash 0
