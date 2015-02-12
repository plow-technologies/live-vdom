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
import           Data.Maybe
-- import           Pipes.Concurrent
import           Prelude                                    hiding (div)

import           Control.Concurrent.STM.Notify

-- GHCJS/VDom/Ophelia
import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM
import           Shakespeare.Dynamic.Render
import           Shakespeare.Ophelia.Parser.VDOM
import           Shakespeare.Ophelia.Parser.VDOM.Event
import qualified VDOM.Adapter                               as VDA

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad                              (forever)
import           Control.Monad.STM
import           Shakespeare.Ophelia

import           Shakespeare.Ophelia.Parser.VDOM.Components

main :: IO ()
main = do
  addCss "style.css"
  container <- createContainer
  mb1@(env1,addr1) <- spawnIO 0
  mb2@(env2,addr2) <- spawnIO 0
  mb3@(env3,addr3) <- spawnIO $ Fired ""
  _ <- forkIO $ forever (modify mb1 >> threadDelay 1000000)
  _ <- forkIO $ forever (modify mb2 >> threadDelay 10000000)
  _ <- forkIO $ onChange env3 (print)
  runDomI container ((showTemp addr3) <$> env3 <*> env1 <*> env2)


addCss :: String -> IO ()
addCss str = [js_|
var ss = document.createElement("link");
ss.type = "text/css";
ss.rel = "stylesheet";
ss.href = `str;
document.getElementsByTagName("head")[0].appendChild(ss);
|]

modify :: (STMEnvelope Int, Address Int) -> IO ()
modify (env, addr) = do
  atomically $ do
    x <- recv env
    send addr $ x + 1
  return ()

  -- forever $ runEffect $ (fromInput $ (\a b -> (a,b)) <$> inp2 <*> inp) >-> printC




-- showTemp :: Address (Event String) -> Event String -> Int -> Int -> LiveVDom VDA.JSEvent
showTemp addr (Fired str) i j  = if j `mod`  2 == 0
                then [gertrude|
                        <div>
                          <div>
                            !{return $ textBox addr Nothing}
                          Will the value update?
                          <div>
                            #{show i}
                          <div>
                            #{show j}
                          <div>
                            #{str}
                      |]
                else [gertrude|
                        <div>
                          <div style="visibility:hidden;">
                            !{return $ textBox addr Nothing}
                          Will the value update?
                          <div>
                            #{show i}
                          <div>
                            #{show j}
                          <div>
                            #{str}
                      |]
showTemp addr Unfired i j = if j `mod`  2 == 0
                then [gertrude|
                        <div>
                          <div>
                            !{return $ textBox addr Nothing}
                          Will the value update?
                          <div>
                            #{show i}
                          <div>
                            #{show j}
                          <div>
                      |]
                else [gertrude|
                        <div>
                          <div style="visibility:hidden;">
                            !{return $ textBox addr Nothing}
                          Will the value update?
                          <div>
                            #{show i}
                          <div>
                            #{show j}
                          <div>
                      |]