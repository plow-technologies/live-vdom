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



-- Produce an infinite stream of increasing integers
produceT :: Producer Integer IO r
produceT = go 0 
  where go n = do
          yield n
          liftIO $ threadDelay 1000000
          go $ n + 1

main :: IO ()
main = do
  container <- createContainer
  (out,inp) <- spawn Unbounded
  _ <- forkIO $ do
    runEffect $ produceT >-> toOutput out
    performGC
  runDomI container (showTemp <$> inp)


showTemp :: Integer -> LiveVDom
showTemp i = [gertrude|
<div>
  Will the value update?
  <div>
    #{show i}
|]