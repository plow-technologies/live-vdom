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
) where


import           Control.Applicative
import           Control.Monad
import           Prelude                               hiding (div)

import           Control.Concurrent.STM.TVar

import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM


import           Shakespeare.Dynamic.Adapter
import qualified VDOM.Adapter                          as VDA

import           Pipes
import           Pipes.Concurrent


import           Shakespeare.Ophelia.Parser.VDOM
import           Shakespeare.Ophelia.Parser.VDOM.Types


runDomI :: IO DOMNode -> Input LiveVDom -> IO ()
runDomI getContainer prd = runDom getContainer (LiveChild prd)

runDom :: IO DOMNode -> LiveVDom -> IO ()
runDom getContainer ld = do
  t <- newTVarIO emptyDiv
  forever $ runEffect $ fromInput (toProducer ld) >-> toSingle >-> (renderDomOn getContainer t)


toSingle :: Monad m => Pipe [a] a m ()
toSingle = do
  x <- await
  case x of
    n:[] -> yield n
    [] -> fail "Unable to have vdom with no main node"
    _ -> fail "Unable to have vdom with more than one main node"

renderDomOn :: IO DOMNode
            -> TVar VNode
            -> Consumer VDA.VNodeAdapter IO ()
renderDomOn getContainer tI = do
  (vna) <- await
  (newNode, oldNode) <- liftIO $ do
    o <- atomically $ readTVar tI
    n <- toVNode vna
    return (n, o)
  let pa = diff oldNode newNode
  _ <- liftIO $ do
    root <- getContainer
    redraw root pa
    atomically $ writeTVar tI newNode
  renderDomOn getContainer tI
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


-- | Create a pipe to render VDom whenever it's updated
renderDom' :: IO DOMNode                      -- ^ Container ov the vdom
          -> VNode                           -- ^ Initial VDom
          -> Consumer (VDA.VNodeAdapter, IO ()) IO () -- ^ Consumer to push VDom to with a finalizer
renderDom' getContainer initial = do
  (vna, f) <- await
  newNode <- liftIO $ toVNode vna
  let pa = diff initial newNode
  _ <- liftIO $ do
    root <- getContainer
    redraw root pa
    f
  renderDom' getContainer newNode

redraw :: DOMNode -> Patch -> IO ()
redraw node pa = pa `seq` atAnimationFrame (patch node pa)

atAnimationFrame :: IO () -> IO ()
atAnimationFrame m = do
  cb <- syncCallback AlwaysRetain False m
  [js_| window.requestAnimationFrame(`cb); |]


