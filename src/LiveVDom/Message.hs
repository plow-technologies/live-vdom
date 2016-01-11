{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LiveVDom.Message (
  Message
, runMessages
, recvMessage
, sendMessage
, modifyMailbox
, debug
, asyncIO
, forkIOMessage
) where


import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM.Notify
import           Control.Monad


-- | A restricted monad for only doing non-blocking
-- message sending and receiving
newtype Message a = Message { unMessage :: IO a }
  deriving (Monad, Functor, Applicative)

-- | Run all messages
runMessages :: Message a -> IO a
runMessages = unMessage

-- | Receive a value from an envelope
recvMessage :: STMEnvelope a -> Message a
recvMessage = Message . recvIO

-- | Send a message to an address
sendMessage :: Address a -> a -> Message ()
sendMessage addr m = Message $ sendIO addr m

-- | Modify the value in a mailbox.
-- This is not atomic
modifyMailbox :: STMMailbox a -> (a -> a)-> Message ()
modifyMailbox (env, addr) f = do
  tg <- recvMessage env
  sendMessage addr $ f tg

-- | Print a String to the console
debug :: String -> Message ()
debug = Message . putStrLn

-- | Run an IO action inside of the Message monad
asyncIO :: IO a -> Message (STMEnvelope (Maybe a))
asyncIO action =
  Message $ do
    (resultEnvelope, resultAddress) <- spawnIO Nothing
    forkIO $ action >>= sendIO resultAddress . Just
    return resultEnvelope

forkIOMessage :: IO () -> Message ()
forkIOMessage = void . asyncIO
