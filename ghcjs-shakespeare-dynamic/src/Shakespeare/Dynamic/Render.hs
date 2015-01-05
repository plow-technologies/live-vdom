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


import           Prelude                     hiding (div)


import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM


import           Shakespeare.Dynamic.Adapter
import qualified VDOM.Adapter                as VDA

import           Pipes



-- | Create a pipe to render VDom whenever it's updated
renderDom :: IO DOMNode                      -- ^ Container ov the vdom
          -> VNode                           -- ^ Initial VDom
          -> Consumer (VDA.VNodeAdapter, IO ()) IO () -- ^ Consumer to push VDom to with a finalizer
renderDom getContainer initial = do
  (vna, finalizer) <- await
  newNode <- liftIO $ toVNode vna
  let pa = diff initial newNode
  _ <- liftIO $ do
    root <- getContainer
    redraw root pa
    finalizer
  renderDom getContainer newNode

redraw :: DOMNode -> Patch -> IO ()
redraw node pa = pa `seq` atAnimationFrame (patch node pa)

atAnimationFrame :: IO () -> IO ()
atAnimationFrame m = do
  cb <- syncCallback AlwaysRetain False m
  [js_| window.requestAnimationFrame(`cb); |]

type LiveDom = (VDA.VNodeAdapter, IO ())