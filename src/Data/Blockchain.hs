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


instance Hashable Word32 where
    putHash = putWord32le


newtype Hash256 = Hash256 ByteString
    deriving (Eq, Ord, Binary)


instance Hashable Hash256 where
    putHash (Hash256 bs) = putByteString bs


instance Show Hash256 where
    show (Hash256 bs) = C8.unpack (B16.encode bs)


initialTarget :: Hash256
initialTarget =
    Hash256 (pack (Prelude.take 8 (allZeros ++ halfZeros ++ allOnes)))
  where
    allOnes = Prelude.repeat maxBound
    halfZeros = [0x0F]
    allZeros = Prelude.replicate 2 minBound


zeroHash :: Hash256
zeroHash = Hash256 (pack (Prelude.replicate 8 minBound))


sha256 :: Hashable a => a -> Hash256
sha256 = Hash256 . SHA256.hash . toStrict . runPut . putHash


data Block a = Block
    { blockHeader :: BlockHeader
    , body        :: a
    } deriving (Show, Eq)


instance Hashable a => Hashable (Block a) where
    putHash (Block bh b) = putHash bh >> putHash b


blockHash :: Hashable a => Block a -> BlockHash
blockHash = BlockHash . sha256


newtype BlockHash = BlockHash Hash256
    deriving (Eq, Binary, Hashable)


instance Show BlockHash where
    show (BlockHash h) = show h


data BlockHeader = BlockHeader
    { prevHash :: BlockHash
    , nonce    :: Word32
    } deriving (Show, Eq)


instance Hashable BlockHeader where
    putHash (BlockHeader p n) =
        put p >> putWord32le n


mineBlock :: Hashable a => Hash256 -> BlockHash -> a -> Block a
mineBlock target parentHash x = L.head minedBlocks
  where
    nonces = [0..]
    blocks = L.map (\n -> Block (BlockHeader parentHash n) x) nonces
    minedBlocks = L.filter (blockSolution target) blocks


blockSolution :: Hashable a => Hash256 -> Block a -> Bool
blockSolution target trialBlock =
    sha256 (sha256 trialBlock) <= target


genesisBlock :: (Num a, Hashable a) => Block a
genesisBlock = mineBlock initialTarget (BlockHash zeroHash) 0


data Blockchain a
    = Genesis (Block a)
    | Node [Block a] (Blockchain a)
    deriving (Show, Eq)
        -- Should be Non-empty List here
        -- Maybe non-empty set?
        -- The ordering (ie. using L.last) is only for finding which came first


printBlockchain :: Show a => Blockchain a -> String
printBlockchain (Node blocks bc) = "Node " ++ show blocks ++ "\n" ++ printBlockchain bc
printBlockchain x = show x


nodeBlocks :: Blockchain a -> [Block a]
nodeBlocks (Genesis x) = [x]
nodeBlocks (Node bs _) = bs


initialChain :: (Hashable a, Num a) => Blockchain a
initialChain = Genesis genesisBlock


valid :: Hashable a => (Block a -> Bool) -> Blockchain a -> Bool
valid p (Genesis x) = p x && blockSolution initialTarget x
valid p (Node blocks bc) = L.all validBlock blocks && valid p bc
  where
    validBlock x =
        p x && blockSolution initialTarget x && prevHashInPrevBlocks x
    prevHashInPrevBlocks b =
        prevHash (blockHeader b) `L.elem` L.map blockHash (nodeBlocks bc)


lookupBlock :: Hashable a => BlockHash -> Blockchain a -> Maybe (Block a)
lookupBlock hash (Genesis block)
    | hash == blockHash block = Just block
lookupBlock hash (Node blocks bc)
    | Just b <- lookupBlockList hash blocks = Just b
    | otherwise = lookupBlock hash bc
lookupBlock _ (Genesis _) = Nothing


blockDepth :: Hashable a => BlockHash -> Blockchain a -> Maybe Int
blockDepth hash (Genesis block)
    | hash == blockHash block = Just 0
blockDepth hash (Node blocks bc)
    | hash `L.elem` L.map blockHash blocks = Just 0
    | Just x <- blockDepth hash bc = Just (x + 1)
blockDepth _ _ = Nothing


lookupBlockList :: Hashable a => BlockHash -> [Block a] -> Maybe (Block a)
lookupBlockList hash = L.find (\b -> hash == blockHash b)


insertBlock :: Hashable a => Block a -> Blockchain a -> Blockchain a
insertBlock b bc | newTip bc = Node [b] bc
  where
    ph = prevHash (blockHeader b)
    newTip (Genesis x) = ph == blockHash x
    newTip (Node bs _) =
        ph `L.elem` L.map blockHash bs
insertBlock b (Node bs bc)
    | not duplicate && validPrev bc = Node (b:bs) bc
  where
    ph = prevHash (blockHeader b)
    duplicate = blockHash b `L.elem` L.map blockHash bs
    validPrev (Genesis x) = ph == blockHash x
    validPrev (Node pbs _) = ph `L.elem` L.map blockHash pbs
insertBlock b (Node bs bc) | not duplicate = Node bs (insertBlock b bc)
  where duplicate = blockHash b `L.elem` L.map blockHash bs
insertBlock _ bc = bc


{- Not very helpful
mineTip :: Hash256 -> Blockchain a -> a -> Block a
mineTip target (Genesis x) = mineBlock target (blockHash x)
mineTip target (Node [x] _) = mineBlock target (blockHash x)
-- Really Nodes should only have non-empty lists
mineTip _ (Node [] _) = error "mineTip: Invalid block, empty."
-- Last of xs because that would be the first one received (see insertBlock)
mineTip target (Node xs _) = mineBlock target (blockHash (L.last xs))
-}


tip :: Blockchain a -> Block a
tip (Genesis x) = x
tip (Node [] _) = error "tip: Invalid node, empty"
tip (Node [x] _) = x
-- Last of xs because that would be the first one received (see insertBlock)
tip (Node xs _) = L.last xs


toList :: Hashable a => Blockchain a -> [Block a]
toList bc@(Genesis x) | valid (const True) bc = [x]
toList (Node [x] xs) | validList listBC = listBC
  where listBC = x : toList' x xs
toList (Node xs ys) | validList listBC = listBC
  where
    listBC = mostRecentValidBlock : toList' mostRecentValidBlock ys
    mostRecentValidBlock = L.head $ L.filter validPrev (L.reverse xs)
    validPrev block = isJust $ ancestor block ys
    isJust (Just _) = True
    isJust _ = False
toList _ = error "toList: Invalid blockchain."

toList' :: Hashable a => Block a -> Blockchain a -> [Block a]
toList' _ bc@(Genesis _) = toList bc
toList' b bc@(Node _ bc') | Just x <- ancestor b bc = x : toList bc'
toList' _ _ = error "toList: Invalid blockchain. Cannot find parent node."


ancestor :: Hashable a => Block a -> Blockchain a -> Maybe (Block a)
ancestor b (Genesis x) | prevHash (blockHeader b) == blockHash x = Just x
ancestor b (Node [x] _) | prevHash (blockHeader b) == blockHash x = Just x
ancestor b (Node xs _)
    | Just x' <- L.find (\x -> prevHash (blockHeader b) == blockHash x) (L.reverse xs)
    = Just x' -- find most recent ancestor
ancestor _ _ = Nothing


validList :: Hashable a => [Block a] -> Bool
validList [x] = blockSolution initialTarget x
validList (x:y:bc) | validBlock = validList (y:bc)
  where validBlock =
            blockSolution initialTarget x &&
            prevHash (blockHeader x) == blockHash y
validList _ = False
