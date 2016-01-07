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

runMessages :: Message a -> IO a
runMessages = unMessage

recvMessage :: STMEnvelope a -> Message a
recvMessage = Message . recvIO

sendMessage :: Address a -> a -> Message ()
sendMessage addr m = Message $ sendIO addr m

modifyMailbox :: STMMailbox a -> (a -> a)-> Message ()
modifyMailbox (env, addr) f = do
  tg <- recvMessage env
  sendMessage addr $ f tg

debug :: String -> Message ()
debug = Message . putStrLn

asyncIO :: IO a -> Message (STMEnvelope (Maybe a))
asyncIO action = 
  Message $ do
    (resultEnvelope, resultAddress) <- spawnIO Nothing
    forkIO $ action >>= sendIO resultAddress . Just
    return resultEnvelope

forkIOMessage :: IO () -> Message ()
forkIOMessage = void . asyncIO