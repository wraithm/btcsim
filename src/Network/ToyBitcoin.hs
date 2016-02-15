module Network.ToyBitcoin where

import           Prelude                          hiding (read)

import qualified Data.List                        as L
import           Data.Maybe                       (fromJust)

import           GHC.Word                         (Word32)

import           Control.Concurrent.Async         (Async, async, cancel,
                                                   waitAny, waitEither)
import           Control.Concurrent.Chan          (Chan, dupChan, newChan)
import           Control.Monad                    (foldM, forever, replicateM,
                                                   when)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict

import           System.IO.Streams                (InputStream, OutputStream)
import qualified System.IO.Streams                as Streams
import qualified System.IO.Streams.Concurrent     as Streams

import           Data.Blockchain


mining
    :: (Hashable a, Show a)
    => Block a
    -> a
    -> IO (Block a)
mining prevBlock x =
    -- putStrLn $ show (x, body prevBlock) ++ " starting mining on: " ++ show prevBlock
    nextBlock `seq` return nextBlock
  where
    nextBlock = mineBlock initialTarget (blockHash prevBlock) x


newtype Conn a = Conn (InputStream a, OutputStream a)


data Server a = Server
    { conn         :: Conn a
    , originalChan :: Chan (Maybe a)
    }


write :: Conn a -> a -> IO ()
write (Conn (_, os)) x = Streams.write (Just x) os


read :: Conn a -> IO (Maybe a)
read (Conn (is, _)) = Streams.read is


server :: IO (Server a)
server = do
    c <- newChan
    is <- Streams.chanToInput c
    os <- Streams.chanToOutput c
    let con = Conn (is, os)
    return $ Server con c


client :: Server a -> IO (Conn a)
client serv = do
    let c = originalChan serv
    c' <- dupChan c
    is <- Streams.chanToInput c'
    os <- Streams.chanToOutput c'
    return $ Conn (is, os)


-- | Node messages
newtype Msg a = NewBlock { unNewBlock :: Block a }


data Peer a = Peer
    { peerId   :: Word32
    , peerConn :: Conn (Msg a)
    }


data Node a = NetNode
    { peers  :: [Peer a]
    , nodeId :: Word32
    }


broadcastBlock :: Node a -> Block a -> IO ()
broadcastBlock (NetNode ps _) b =
    mapM_ broadcastToPeer ps
  where
    broadcastToPeer (Peer _ pconn) =
        -- putStrLn $ "Sending block from " ++ show nid ++ " to " ++ show pid
        write pconn (NewBlock b)


type BlockStream a = InputStream (Block a)


blockStream :: [Peer a] -> IO (BlockStream a)
blockStream ps = Streams.map unNewBlock =<< Streams.concurrentMerge (map iss ps)
  where iss (Peer _ (Conn (is, _))) = is


listenBlocks :: BlockStream a -> IO (Block a)
listenBlocks = fmap fromJust . Streams.read


connectPeer :: Node a -> (Word32, Server (Msg a)) -> IO (Node a)
connectPeer n (pid, serv) = do
    p <- Peer pid <$> client serv
    return n { peers = p:peers n }


data MiningState a = MiningState
    { miner       :: Async (Block a)
    , listener    :: Async (Block a)
    , activeChain :: Blockchain a
    }


setMiner :: Async (Block a) -> StateT (MiningState a) IO ()
setMiner m = do
    oldMiner <- gets miner
    lift $ cancel oldMiner
    modify' (\ms -> ms { miner = m })


setActiveChain :: Monad m => Blockchain a -> StateT (MiningState a) m ()
setActiveChain bc = modify (\ms -> ms { activeChain = bc })


resetListener :: BlockStream a -> StateT (MiningState a) IO ()
resetListener bs = do
    oldListener <- gets listener
    lift $ cancel oldListener
    newListener <- lift $ async $ listenBlocks bs
    modify' (\ms -> ms { listener = newListener })


-- | Word32 for nodeId
miningNode :: Blockchain Word32 -> Node Word32 -> IO ()
miningNode initBC n@(NetNode ps nid) = do
    -- Listen stream for blocks
    bs <- blockStream ps
    initListener <- async $ listenBlocks bs

    -- start miner off of first block
    initMiner <- async (mining (tip initBC) nid)

    flip evalStateT (MiningState initMiner initListener initBC) $ forever $ do
        miningAsync <- gets miner
        recvAsync <- gets listener
        -- Wait for either recv or found block
        nextBlock <- lift $ waitEither recvAsync miningAsync
        -- lift $ logNewBlock nextBlock

        -- Process the block
        bc <- gets activeChain
        handleBlock bs bc nextBlock
  where
    handleBlock bs bc (Left recvBlock)
        | Just depth <- blockDepth (prevHash (blockHeader recvBlock)) bc
        , depth < 6 = do -- Check that the block isn't super stale
        let prevBlock = tip bc
        -- TODO Check that the block is valid?
        setActiveChain $ insertBlock recvBlock bc
        resetListener bs
        when (recvNew recvBlock prevBlock) $ do
            -- lift $ putStrLn "Received new block!!!"
            newMiner <- lift $ async $ mining recvBlock nid
            setMiner newMiner
    handleBlock bs _ (Left _) = resetListener bs -- lift $ putStrLn "Stale block..."
    handleBlock _ _ (Right foundBlock) = do
        -- This assumes the block will eventually make it back and get processed
        -- This also sort of assumes it will only get called once, whatever, block spam is normal
        lift $ broadcastBlock n foundBlock
        m <- gets miner
        lift $ cancel m

    logNewBlock (Left b) = putStrLn $
        "Node " ++ show nid ++ " received new block: " ++ show b
    logNewBlock (Right b) = putStrLn $ "Found new block: " ++ show b


recvNew :: Hashable a => Block a -> Block a -> Bool
recvNew nextBlock prevBlock = prevHash (blockHeader nextBlock) == blockHash prevBlock


node :: Blockchain Word32 -> Node Word32 -> IO ()
node initBC (NetNode ps _) = do
    bs <- blockStream ps
    flip evalStateT initBC $ forever $ do
        nextBlock <- lift $ listenBlocks bs
        bc <- get
        handleBlock bc nextBlock
        let longestChain = toList bc
        lift $ putStrLn $ unlines $ map show longestChain
        lift $ print (validList longestChain)
        -- lift $ putStrLn $ printBlockchain bc
        -- lift $ print (tip bc)
        -- lift $ putStrLn $ "Listen-only node " ++ show nid ++ " received new block (" ++ show (blockHash nextBlock) ++ "): " ++ show nextBlock
        -- lift $ putStrLn "\n"
  where
    handleBlock bc recvBlock
        | Just depth <- blockDepth (prevHash (blockHeader recvBlock)) bc
        , depth < 6 = put $ insertBlock recvBlock bc
    handleBlock _ _ = lift $ putStrLn "Listen-only stale block..."


completeNetwork :: Int -> IO [Node Word32]
completeNetwork numNodes = do
    servers <- replicateM numNodes server
    let idServers = zip nodeIds servers
    mapM (createNode idServers) nodeIds
  where
    nodeIds = [1..(fromIntegral numNodes)]
    emptyNode = NetNode []
    createNode :: [(Word32, Server (Msg a))] -> Word32 -> IO (Node a)
    createNode servers nid = do
        let peerServers = filter ((/= nid) . fst) servers
        foldM connectPeer (emptyNode nid) peerServers


startCompleteNetwork :: Int -> IO ()
startCompleteNetwork numNodes = (initialChain :: Blockchain Word32) `seq` do
    nodes <- completeNetwork numNodes
    let (n, m) = L.splitAt 1 nodes
    listenAsyncs <- mapM (async . node initialChain) n
    miningAsyncs <- mapM (async . miningNode initialChain) m
    _ <- waitAny (listenAsyncs ++ miningAsyncs)
    return ()
