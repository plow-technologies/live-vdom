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
, LiveDom
) where


import           Control.Monad
import           Prelude                               hiding (div)

import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM


import           Shakespeare.Dynamic.Adapter
import qualified VDOM.Adapter                          as VDA

import           Pipes
import           Pipes.Concurrent


import           Shakespeare.Ophelia.Parser.VDOM
import           Shakespeare.Ophelia.Parser.VDOM.Types




runDom :: IO DOMNode -> LiveVDom-> IO ()
runDom getContainer ld = do
  forever $ runEffect $ fromInput (toProducer ld) >-> toSingle >-> (renderDom getContainer emptyDiv)


toSingle :: Monad m => Pipe [a] a m ()
toSingle = do
  x <- await
  case x of
    x:[] -> yield x
    [] -> fail "Unable to have vdom with no main node"
    _ -> fail "Unable to have vdom with more than one main node"

-- | Create a pipe to render VDom whenever it's updated
renderDom :: IO DOMNode                      -- ^ Container ov the vdom
          -> VNode                           -- ^ Initial VDom
          -> Consumer (VDA.VNodeAdapter) IO () -- ^ Consumer to push VDom to with a finalizer
renderDom getContainer initial = do
  (vna) <- await
  newNode <- liftIO $ toVNode vna
  let pa = diff initial newNode
  _ <- liftIO $ do
    root <- getContainer
    redraw root pa
  renderDom getContainer newNode

redraw :: DOMNode -> Patch -> IO ()
redraw node pa = pa `seq` atAnimationFrame (patch node pa)

atAnimationFrame :: IO () -> IO ()
atAnimationFrame m = do
  cb <- syncCallback AlwaysRetain False m
  [js_| window.requestAnimationFrame(`cb); |]

type LiveDom = (VDA.VNodeAdapter, IO ())
