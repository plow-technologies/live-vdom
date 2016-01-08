{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LiveVDom.Internal(
    VDom
  , setVDom
  , buildVDom
  , child
  , children
  , spawnVDom
) where

import           Control.Concurrent.STM.Notify
import           Control.Monad.State.Strict

import qualified Data.Sequence                 as S

import qualified LiveVDom.Types                as T
import           LiveVDom.UserTypes



newtype VDom a = VDom { unVDom :: StateT LiveVDom IO a }
  deriving (Functor, Monad, MonadFix, Applicative, MonadIO)

setVDom :: LiveVDom -> VDom ()
setVDom = VDom . put

modifyVDom :: (LiveVDom -> LiveVDom) -> VDom ()
modifyVDom = VDom . modify

buildVDom :: VDom () -> IO LiveVDom
buildVDom (VDom state) = execStateT state (T.LiveVNode [] "div" [] S.empty)

child :: VDom () -> VDom ()
child inner = do
  c <- liftIO $ buildVDom inner
  modifyVDom (T.addChild c)

children :: S.Seq (VDom ()) -> VDom ()
children inners = do
  cs <- liftIO $ traverse buildVDom inners
  modifyVDom (T.addChildren cs)

spawnVDom :: a -> VDom (STMMailbox a)
spawnVDom = VDom . liftIO . spawnIO

recvVDom :: STMEnvelope a -> VDom a
recvVDom = VDom . liftIO . recvIO

sendVDom :: a -> Address a -> VDom ()
sendVDom val addr = VDom . liftIO $ sendIO addr val