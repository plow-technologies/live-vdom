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
import qualified Data.Sequence                         as S
import           Prelude                               hiding (div)


import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM


import           Shakespeare.Dynamic.Adapter
import qualified VDOM.Adapter                          as VDA

import           Pipes
-- import           Pipes.Concurrent  -- Not used because of stm-notify
import           Control.Concurrent.STM.Notify

import           Shakespeare.Ophelia.Parser.VDOM.Types


-- | Run dom (not forked) forever. This receives the current dom
-- and then renders it again each time it changes
runDomI :: DOMNode -- ^ Container to render the dom in
        -> IO ()   -- ^ Action to run after the FIRST render
        -> STMEnvelope (LiveVDom VDA.JSEvent)  -- ^ dom to run and watch for changes
        -> IO ()
runDomI container postRun envLD = do
  putStrLn "Running"
  vdm <- recvIO envLD
  putStrLn "Received"
  vn' <- renderDom container emptyDiv vdm          -- Render the initial dom
  putStrLn "Rendered"
  _ <- atAnimationFrame postRun
  putStrLn "Folding"
  foldOnChangeWith waitForDom envLD (renderDom container) vn'    -- pass the rendered dom into the fold that
                                                   -- renders the dom when it changes

-- | Run the dom inside a container that
runDom :: DOMNode
      -> IO ()
      -> (LiveVDom VDA.JSEvent)
      -> IO ()
runDom c fi e = runDomI c fi $ return e


-- | Given a container, the last rendering, and a current rendering,
-- diff the new rendering from the old and return the new model of the dom
renderDom :: DOMNode -> VNode -> (LiveVDom VDA.JSEvent) -> IO VNode
renderDom container old ld = do
  let vna = toProducer ld
  vnaL <- recvIO vna
  vna' <- if S.length vnaL > 1
    then fail "Having more than one node as the parent is illegal"
    else return $ S.index vnaL 0
  new <- toVNode vna'
  let pa = diff old new
  redraw container pa
  return new


-- | create an empty div to run dom inside of and add it to the
-- body of the document
createContainer :: IO DOMNode
createContainer = do
  container <- [js| document.createElement('div'); |] :: IO DOMNode
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

-- | Redraw the dom using a patch
redraw :: DOMNode -> Patch -> IO ()
redraw node pa = pa `seq` atAnimationFrame (patch node pa)

-- | Use the window requestAnimation frame
-- to run some IO action when able to
atAnimationFrame :: IO () -> IO ()
atAnimationFrame m = do
  cb <- syncCallback NeverRetain False m
  [js_| window.requestAnimationFrame(`cb); |]


