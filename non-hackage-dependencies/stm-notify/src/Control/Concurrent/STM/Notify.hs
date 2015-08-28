{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
module Control.Concurrent.STM.Notify (
    STMEnvelope
  , Address
  , spawnIO
  , spawn
  , spawnEnvelope
  , recvIO
  , recv
  , sendIO
  , send
  , forkOnChange
  , onChange
  , foldOnChange
  , STMMailbox
  , notify
  , waitForChanges
  , watchOn
  , addListener
  , foldOnChangeWith
)where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad            hiding (mapM, sequence)
import           Data.Traversable         (traverse)
import           Prelude                  hiding (mapM, sequence)


type STMMailbox a = (STMEnvelope a, Address a)

data STMEnvelope a = STMEnvelope {
  stmEnvelopeTMvar :: STM ([TMVar ()],a) -- ^ Current list of waiting listeners and the current value
, stmAddListener   :: TMVar () -> STM () -- ^ Add a listener to the envelope
}


-- A wrapper to send an item to its paried container
newtype Address a = Address { _unAddress :: a -> STM () }


instance Functor STMEnvelope where
  fmap f (STMEnvelope stmVal insertListener) = STMEnvelope newStmVal newInsertListener
    where newInsertListener = insertListener -- Keep the same listener
          newStmVal = fmap f <$> stmVal -- Apply f to the current value
                                          -- (<$> over STM and fmap over the tuple)

instance Applicative STMEnvelope where
  pure r = STMEnvelope (return ([], r)) (const $ return ())
  -- Empty list of listeners and a function that doesn't add anything
  -- Because there is no way to modify this value by default
  (STMEnvelope env1 insertListener1) <*> (STMEnvelope env2 insertListener2) = STMEnvelope newStmVal newAddListener
    where newStmVal = (\(fList,f) (valList, val) -> (fList ++ valList, f val)) <$> env1 <*> env2 -- Apply the function to the value
          newAddListener t = insertListener1 t >> insertListener2 t -- Use both listeners to allow for listening to multiple changes


instance Alternative STMEnvelope where
  empty = STMEnvelope empty (const $ return ())
  (STMEnvelope val listener) <|> (STMEnvelope val' listener') = STMEnvelope (val <|> val') (\t -> listener t >> listener' t)

instance Monad STMEnvelope where
  return r = STMEnvelope (return ([], r)) (const $ return ())
  (STMEnvelope stmVal insertListener) >>= f = STMEnvelope stmNewVal newInsertListener
    where stmNewVal = fst <$> updateFcnVal
          newInsertListener = fixAddFunc $ snd <$> updateFcnVal
          updateFcnVal = do
            (listeners, currentVal) <- stmVal
            let (STMEnvelope stmRes insertListener') = f currentVal
                add t = insertListener' t >> insertListener t
            (listeners', newVal) <- stmRes
            return ((listeners' ++ listeners, newVal), add)
          fixAddFunc func tm =  ($ tm) =<< func

-- | Spawn a new envelope and an address to send new data to
spawnIO :: a -> IO (STMEnvelope a, Address a)
spawnIO = atomically . spawn

-- | Spawn a new envelope and address inside of an envelope computation
spawnEnvelope :: a -> STMEnvelope (STMEnvelope a, Address a)
spawnEnvelope x = STMEnvelope (([],) <$> spawned) addListener
  where spawned = spawn x
        addListener = fixStm (stmAddListener . fst <$> spawned)
        fixStm f x = ($ x) =<< f

-- | Spawn a new envelope and an address to send new data to
spawn :: a -> STM (STMEnvelope a, Address a)
spawn val = do
  tValue <- newTMVar ([],val)                                -- Contents with no listeners
  let envelope = STMEnvelope (readTMVar tValue) insertListener  -- read the current value and an add function
      insertListener listener = do
        (listeners, a) <- takeTMVar tValue                   -- Get the list of listeners to add
        putTMVar tValue (listener:listeners, a)              -- append the new listener
      address = Address $ \newVal -> do
        (listeners,_) <- takeTMVar tValue                    -- Find the listeners
        putTMVar tValue ([],newVal)                        -- put the new value with no listeners
        mapM_ (`tryPutTMVar`  ()) listeners                 -- notify all the listeners of the change
  return (envelope, address)


-- | Force a notification event. This doesn't clear the listeners
notify :: STMEnvelope a -> STM ()
notify (STMEnvelope stmVal _) = do
  (listeners, _) <- stmVal                                   -- Get a list of all current listeners
  mapM_ (`tryPutTMVar` ()) listeners                       -- Fill all of the tmvars


-- | Read the current contents of a envelope
recvIO :: STMEnvelope a -> IO a
recvIO = atomically . recv

-- | Read the current contents of a envelope
recv :: STMEnvelope a -> STM a
recv = fmap snd . stmEnvelopeTMvar

-- | Update the contents of a envelope for a specific address
-- and notify the watching thread
sendIO :: Address a -> a -> IO ()
sendIO m v = atomically $ send m v

-- | Update the contents of a envelope for a specific address
-- and notify the watching thread
send :: Address a -> a -> STM ()
send (Address sendF) = sendF

-- | Watch the envelope in a thread. This is the only thread that
-- can watch the envelope. This never ends
forkOnChange :: STMEnvelope a -- ^ Envelope to watch
             -> (a -> IO b)  -- ^ Action to perform
             -> IO (Async b) -- ^ Resulting async value so that you can cancel
forkOnChange v f = async $ onChange v f

-- | Watch the envelope for changes. This never ends
onChange :: STMEnvelope a -- ^ Envelope to watch
         -> (a -> IO b)  -- ^ Action to perform
         -> IO b
onChange env f = forever $ waitForChange env >> (f =<< recvIO env)

onChangeWith :: (a -> [STMEnvelope a])
             -> STMEnvelope a
             -> (a -> IO b)
             -> IO ()
onChangeWith children env f = void . forever $ do
  watch <- newEmptyTMVarIO
  go children watch env
  atomically $ readTMVar watch
  _ <- f =<< recvIO env
  return ()
    where go getChildren watch env = do
            current <- recvIO env
            atomically $ stmAddListener env watch
            mapM_ (go getChildren watch) $ getChildren current

forkOnChangeWith :: (a -> [STMEnvelope a])
                 -> STMEnvelope a
                 -> (a -> IO b)
                 -> IO (Async ())
forkOnChangeWith getC env f = async $ onChangeWith getC env f

waitForChange :: STMEnvelope a -> IO ()
waitForChange (STMEnvelope _ insertListener) = do
  x <- newEmptyTMVarIO
  atomically $ insertListener x            -- This is two seperate transactions because readTMVar will fail
  atomically $ readTMVar x              -- Causing insertListener to retry


waitForChanges :: (a -> [a]) -> (a -> STMEnvelope b) -> a -> IO ()
waitForChanges getChildren getEnv start = do
  listener <- newEmptyTMVarIO
  go listener getChildren getEnv start
  where go listener getCh env val = do
              let insertListener = stmAddListener $ getEnv start
              atomically $ insertListener listener
              mapM_ (go listener getCh env) $ getCh val



-- -- | fold across a value each time the envelope is updated
foldOnChange :: STMEnvelope a     -- ^ Envelop to watch
             -> (b -> a -> IO b)  -- ^ fold like function
             -> b                 -- ^ Initial value
             -> IO ()
foldOnChange = foldOnChangeWith waitForChange



foldOnChangeWith :: (STMEnvelope a -> IO ()) -- ^ Function to wait for a change
             -> STMEnvelope a     -- ^ Envelop to watch
             -> (b -> a -> IO b)  -- ^ fold like function
             -> b                 -- ^ Initial value
             -> IO ()
foldOnChangeWith waitFunc env fld accum = do
  _ <- waitFunc env
  val <- recvIO env -- wait for the lock and then read the value
  accum' <- fld accum val
  foldOnChangeWith waitFunc env fld accum'


watchOn :: (a -> STMEnvelope [a]) -> STMEnvelope a -> IO ()
watchOn f stmVal = do
  listener <- newEmptyTMVarIO
  atomically $ stmAddListener stmVal listener
  currentVal <- recvIO stmVal
  let envCh = f currentVal
  children <- recvIO envCh
  mapM_ (go listener f) children
  atomically $ readTMVar listener
  where go listener func val = do
          let envChildren = func val
          atomically $ stmAddListener envChildren listener
          ch <- recvIO envChildren
          mapM_ (go listener func) ch

addListener :: STMEnvelope a -> TMVar () -> STM ()
addListener = stmAddListener
