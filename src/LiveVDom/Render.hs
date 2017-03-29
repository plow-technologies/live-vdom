{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}

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
import           Prelude                       hiding (div)


import           GHCJS.Foreign.QQ
import           GHCJS.VDOM


import           Control.Concurrent.STM.Notify
import           LiveVDom.Adapter              (mkVNode)

import           GHCJS.Foreign.Callback
import           GHCJS.VDOM.Element
import qualified GHCJS.VDOM.Event              as EV
import           JavaScript.Web.AnimationFrame (inAnimationFrame)
import           LiveVDom.Types                hiding (LiveVDom)
import           LiveVDom.UserTypes

import LiveVDom.Internal

-- | Run dom (not forked) forever. This receives the current dom
-- and then renders it again each time it changes
runDomI :: DOMNode -- ^ Container to render the dom in
        -> IO ()   -- ^ Action to run after the first render
        -> Elem IO -- ^ dom to run and watch for changes
        -> IO ()
runDomI container postRun element = do
  EV.initEventDelegation EV.defaultEvents -- need this for events to work
  (vdm, _) <- renderElement element
  vmount <- mount container $ div () ()
  vn' <- renderDom vmount vdm          -- Render the initial dom
  _ <- inAnimationFrame ContinueAsync (\_ -> postRun)
  foldOnChangeWith waitForDom (return vdm) (\_ v -> renderDom vmount v) vn'    -- pass the rendered dom into the fold that
                                                              -- renders the dom when it changes

-- | Run the dom inside a container that
runDom :: DOMNode
      -> IO ()
      -> Elem Identity
      -> IO ()
runDom container postRun element = do
  EV.initEventDelegation EV.defaultEvents
  let (vdm, _) = runIdentity $ renderElement element
  vmount <- mount container $ div () ()
  vn' <- renderDom vmount vdm
  _ <- inAnimationFrame ContinueAsync (\_ -> postRun)
  foldOnChangeWith waitForDom (return vdm) (\_ v -> renderDom vmount v) vn'    -- pass the rendered dom into the fold that

-- | Given a container, the last rendering, and a current rendering,
-- diff the new rendering from the old and return the new model of the dom
renderDom :: VMount -> LiveVDom -> IO ()
renderDom vMount !ld = do
  !vns <- mkVNode ld
  !new <- case vns of
            (x:[]) -> return x
            _ -> fail "Having more than one node as the parent is illegal"
  !pa <- diff vMount new
  _ <- patch vMount pa
  putStrLn "Rendered"
  return ()


-- | create an empty div to run dom inside of and add it to the
-- body of the document
createContainer :: IO DOMNode
createContainer = do
  container <- [js| document.createElement('div') |] :: IO DOMNode
  [js_| document.body.appendChild(`container); |] :: IO ()
  return container
