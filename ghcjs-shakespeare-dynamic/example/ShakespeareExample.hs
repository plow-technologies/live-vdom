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
import           Control.Monad                   (forever)
import           Data.Maybe
import           Data.String.Here
import qualified Data.Text                       as T
import           Pipes
import           Pipes.Concurrent
import           Prelude                         hiding (div)

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

import           Control.Concurrent.STM.TVar
import           Shakespeare.Ophelia



-- An example of a VDom producer.
-- This could take multiple inputs and yeidl vdom when necessary or
-- you could use the functor instance on Output in Pipe concurrent
produceT :: Producer Integer IO r
produceT = do
  tm <- liftIO $ newTVarIO 0
  forever $ do
    x' <- liftIO $ atomically $ do
            t <- readTVar tm
            writeTVar tm (t+1)
            return t
    yield x'
    liftIO $ threadDelay 1000000

main :: IO ()
main = do
  container <- [js| document.createElement('div') |] :: IO DOMNode         -- Container to run the dom inside of
  [js_| document.body.appendChild(`container); |] :: IO ()                 -- Add the container
  (out,inp) <- spawn Unbounded
  _ <- forkIO $ do
    runEffect $ produceT >-> toOutput out
    performGC
  let getContainer = [js|document.body.childNodes[3]|]                     -- Should be a better way to get the container
  runDomI getContainer (showTemp <$> inp)


showTemp :: Integer -> LiveVDom
showTemp i = [gertrude|
<div>
  Will the value update?
  <div>
    #{show i}
|]