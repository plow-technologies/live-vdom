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
-- import           Pipes.Concurrent  -- Not used because of stm-notify
import           Control.Concurrent.STM.Notify


import           Shakespeare.Ophelia.Parser.VDOM
import           Shakespeare.Ophelia.Parser.VDOM.Types


runDomI :: DOMNode -> STMEnvelope LiveVDom -> IO ()
runDomI container envLD = foldOnChange  envLD (renderDom container) emptyDiv


runDom :: DOMNode -> LiveVDom -> IO ()
runDom c e = runDomI c $ return e 

renderDom :: DOMNode -> VNode -> LiveVDom -> IO VNode
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

-- runDomI :: DOMNode -> Input LiveVDom -> IO ()
-- runDomI container prd = runDom container (LiveChild prd)

-- runDom :: DOMNode -> LiveVDom -> IO ()
-- runDom container ld = do
--   t <- newTVarIO emptyDiv
--   forever $ runEffect $ fromInput (toProducer ld) >-> toSingle >-> (renderDomOn container t)


toSingle :: Monad m => Pipe [a] a m ()
toSingle = do
  x <- await
  case x of
    n:[] -> yield n
    [] -> fail "Unable to have vdom with no main node"
    _ -> fail "Unable to have vdom with more than one main node"

-- renderDomOn :: DOMNode
--             -> TVar VNode
--             -> Consumer VDA.VNodeAdapter IO ()
-- renderDomOn container tI = do
--   (vna) <- await
--   (newNode, oldNode) <- liftIO $ do
--     o <- atomically $ readTVar tI
--     n <- toVNode vna
--     return (n, o)
--   let pa = diff oldNode newNode
--   _ <- liftIO $ do
--     redraw container pa
--     atomically $ writeTVar tI newNode
--   renderDomOn container tI
-- | Create a pipe to render VDom whenever it's updated
-- renderDom :: DOMNode                      -- ^ Container ov the vdom
--           -> VNode                           -- ^ Initial VDom
--           -> Consumer (VDA.VNodeAdapter) IO () -- ^ Consumer to push VDom to with a finalizer
-- renderDom container initial = do
--   (vna) <- await
--   newNode <- liftIO $ toVNode vna
--   let pa = diff initial newNode
--   _ <- liftIO $ do
--     redraw container pa
--   renderDom container newNode


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


