{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-
  Straight stolt on from virtual-dom
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}


module Shakespeare.Dynamic.Render (
  renderDom
, renderDom'
, runDom
, runDomI
, createContainer
) where


import           Control.Monad
import           Prelude                               hiding (div)


import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM


import           Shakespeare.Dynamic.Adapter
import qualified VDOM.Adapter                          as VDA

import           Pipes
-- import           Pipes.Concurrent  -- Not used because of stm-notify
import           Control.Concurrent.STM.Notify
import Control.Concurrent

import           Shakespeare.Ophelia.Parser.VDOM.Types


runDomI :: DOMNode -> IO () -> STMEnvelope (LiveVDom VDA.JSEvent) -> IO ()
runDomI container postRun envLD = do
  vdm <- recvIO envLD
  vn' <- renderDom container emptyDiv vdm          -- Render the initial dom
  _ <- atAnimationFrame postRun
  foldOnChange  envLD (renderDom container) vn'    -- pass the rendered dom into the fold that
                                                   -- renders the dom when it changes


runDom :: DOMNode -> IO () -> (LiveVDom VDA.JSEvent) -> IO ()
runDom c fi e = runDomI c fi $ return e 

renderDom :: DOMNode -> VNode -> (LiveVDom VDA.JSEvent) -> IO VNode
renderDom container old ld = do
  let vna = toProducer ld
  vnaL <- recvIO vna
  vna' <- if length vnaL > 1
    then fail "Having more than one node as the parent is illegal"
    else return $ vnaL !! 0
  new <- toVNode vna'
  let pa = diff old new
  redraw container pa
  return new

createContainer :: IO DOMNode
createContainer = do
  container <- [js| document.createElement('div') |] :: IO DOMNode
  [js_| document.body.appendChild(`container); |] :: IO ()
  return container


-- | Create a pipe to render VDom whenever it's updated
renderDom' :: DOMNode                      -- ^ Container ov the vdom
          -> VNode                           -- ^ Initial VDom
          -> Consumer (VDA.VNodeAdapter, IO ()) IO () -- ^ Consumer to push VDom to with a finalizer
renderDom' container initial = do
  (vna, f) <- await
  newNode <- liftIO $ toVNode vna
  let pa = diff initial newNode
  _ <- liftIO $ do
    redraw container pa
    f
  renderDom' container newNode

redraw :: DOMNode -> Patch -> IO ()
redraw node pa = pa `seq` atAnimationFrame (patch node pa)

atAnimationFrame :: IO () -> IO ()
atAnimationFrame m = do
  cb <- syncCallback NeverRetain False m
  [js_| window.requestAnimationFrame(`cb); |]


