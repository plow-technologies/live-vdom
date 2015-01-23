{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-
  Straight stolt on from virtual-dom
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}


module Main where

-- Base
import           Control.Applicative
import           Control.Concurrent              (threadDelay)
import           Control.Monad                   (forever, liftM2)
import           Data.Maybe
import           Data.String.Here
import qualified Data.Text                       as T
import           Pipes
-- import           Pipes.Concurrent
import           Prelude                         hiding (div)

import           Control.Concurrent.STM.Notify

-- Used for importing parsing because
-- The quasiquoting isn't done
import qualified Text.Trifecta.Result            as R

-- GHCJS/VDom/Ophelia
import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM
import           Shakespeare.Dynamic.Render
import           Shakespeare.Ophelia.Parser.VDOM
import qualified VDOM.Adapter                    as VDA

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Shakespeare.Ophelia

import Shakespeare.Ophelia.Parser.VDOM.Components
main :: IO ()
main = do
  container <- createContainer
  mb1@(env1,addr1) <- spawnIO 0
  mb2@(env2,addr2) <- spawnIO 0
  _ <- forkIO $ forever (modify mb1 >> threadDelay 10000)
  _ <- forkIO $ forever (modify mb2 >> threadDelay 10000000)
  runDomI container (showTemp <$> env1 <*> env2)



modify :: (STMEnvelope Int, Address Int) -> IO ()
modify (env, addr) = do
  atomically $ do
    x <- recv env
    send addr $ x + 1
  return ()

  -- forever $ runEffect $ (fromInput $ (\a b -> (a,b)) <$> inp2 <*> inp) >-> printC




showTemp :: Int -> Int -> LiveVDom VDA.JSEvent
showTemp i j = [gertrude|
<div>
  Will the value update?
  <div>
    #{show i}
  <div>
    #{show j}
  <div>
    !{return $ button undefined "hello"}
|]
