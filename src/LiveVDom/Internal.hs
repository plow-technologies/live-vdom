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


-- | A monad for composing two seperate modules without declaring
-- all mutable pieces at the top level
newtype VDom a = VDom { unVDom :: StateT LiveVDom IO a }
  deriving (Functor, Monad, MonadFix, Applicative, MonadIO)

-- | Sequence the internal actions of StateT to build dom
buildVDom :: VDom () -> IO LiveVDom
buildVDom (VDom state) = execStateT state (T.LiveVNode [] "div" [] S.empty)

-- | Set the current level for vdom
setVDom :: LiveVDom -> VDom ()
setVDom = VDom . put

-- | Used internally to modify the current vdom
modifyVDom :: (LiveVDom -> LiveVDom) -> VDom ()
modifyVDom = VDom . modify

-- | Insert a child into the current element
child :: VDom () -> VDom ()
child inner = do
  c <- liftIO $ buildVDom inner
  modifyVDom (T.addChild c)

-- | Insert a sequence of children into the current dom
children :: S.Seq (VDom ()) -> VDom ()
children inners = do
  cs <- liftIO $ traverse buildVDom inners
  modifyVDom (T.addChildren cs)

-- | Spawn a mailbox for mutable values
spawnVDom :: a -> VDom (STMMailbox a)
spawnVDom = VDom . liftIO . spawnIO
