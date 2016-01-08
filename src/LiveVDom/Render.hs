{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BangPatterns #-}

{-
  Straight stolt on from virtual-dom
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}


module LiveVDom.Render (
  renderDom
, runDom
, runDomI
, createContainer
) where


import           Control.Monad
import qualified Data.Sequence                         as S
import           Prelude                               hiding (div)


import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM
import           GHCJS.Types


import           LiveVDom.Adapter (mkVNode, debugDom)
import qualified LiveVDom.Adapter.Types                          as VDA
import           Control.Concurrent
import           Control.Concurrent.STM.Notify

import           LiveVDom.Types hiding (LiveVDom)
import           LiveVDom.UserTypes
import qualified GHCJS.VDOM.Event as EV
import           GHCJS.VDOM.Element
import           GHCJS.Foreign.Callback
import           JavaScript.Web.AnimationFrame (inAnimationFrame)


-- Don't leave this in
import Unsafe.Coerce
import qualified GHCJS.VDOM.Element as E
import Data.JSString (pack)

-- | Run dom (not forked) forever. This receives the current dom
-- and then renders it again each time it changes
runDomI :: DOMNode -- ^ Container to render the dom in
        -> IO ()   -- ^ Action to run after the FIRST render
        -> STMEnvelope LiveVDom -- ^ dom to run and watch for changes
        -> IO ()
runDomI container postRun envLD = do
  EV.initEventDelegation EV.defaultEvents -- need this for events to work
  vdm <- recvIO envLD
  vmount <- mount container $ div () ()
  vn' <- renderDom vmount vdm          -- Render the initial dom
  _ <- inAnimationFrame ContinueAsync (\_ -> postRun)
  foldOnChangeWith waitForDom envLD (\_ v -> renderDom vmount v) vn'    -- pass the rendered dom into the fold that
                                                              -- renders the dom when it changes

-- | Run the dom inside a container that
runDom :: DOMNode
      -> IO ()
      -> LiveVDom
      -> IO ()
runDom c fi e = runDomI c fi $ return e

-- | Given a container, the last rendering, and a current rendering,
-- diff the new rendering from the old and return the new model of the dom
renderDom :: VMount -> LiveVDom -> IO ()
renderDom mount !ld = do
  !vns <- mkVNode ld
  !new <- case vns of
            (x:[]) -> return x
            _ -> fail "Having more than one node as the parent is illegal"
  !pa <- diff mount new
  _ <- patch mount pa
  putStrLn "Rendered"
  return ()


-- | create an empty div to run dom inside of and add it to the
-- body of the document
createContainer :: IO DOMNode
createContainer = do
  container <- [js| document.createElement('div') |] :: IO DOMNode
  [js_| document.body.appendChild(`container); |] :: IO ()
  return container