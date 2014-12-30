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
) where


import           Prelude                     hiding (div)

import           System.IO

import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM


import           Shakespeare.Dynamic.Adapter
import qualified VDOM.Adapter                as VDA

import           Pipes

renderDom :: IO DOMNode -> VNode -> IO a -> Consumer VDA.VNodeAdapter IO ()
renderDom getContainer initial finalizer = do
  vna <- await
  newNode <- liftIO $ toVNode vna
  let pa = diff initial newNode
  liftIO $ do
    root <- getContainer
    redraw root pa
    finalizer
  renderDom getContainer newNode finalizer

redraw :: DOMNode -> Patch -> IO ()
redraw node pa = pa `seq` atAnimationFrame (patch node pa)

atAnimationFrame :: IO () -> IO ()
atAnimationFrame m = do
  cb <- syncCallback AlwaysRetain False m
  [js_| window.requestAnimationFrame(`cb); |]
