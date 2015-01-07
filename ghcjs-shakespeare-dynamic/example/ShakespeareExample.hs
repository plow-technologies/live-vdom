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





-- An example of a VDom producer. 
-- This could take multiple inputs and yeidl vdom when necessary or
-- you could use the functor instance on Output in Pipe concurrent
produceVDom :: Producer LiveDom IO r
produceVDom = forever $ do
  yield (v1,return ())
  lift $ threadDelay 2000000
  yield (v2,return ())
  lift $ threadDelay 2000000
  where (v1,v2) = filterRes


emptyOut :: IO (Output LiveDom, Input LiveDom)
emptyOut = spawn Unbounded


main :: IO ()
main = do
  container <- [js| document.createElement('div') |] :: IO DOMNode         -- Container to run the dom inside of
  [js_| document.body.appendChild(`container); |] :: IO ()                 -- Add the container
  (out,inp) <- emptyOut
  _ <- forkIO $ do
    runEffect $ produceVDom >-> toOutput out
    performGC
  let getContainer = [js|document.body.childNodes[1]|]                     -- Should be a better way to get the container
  forever $ runEffect $ fromInput inp >-> (renderDom getContainer emptyDiv)


buildPropS :: String -> T.Text -> VDA.Property
buildPropS = VDA.buildProp


addOnClick :: VNode -> IO VNode
addOnClick (VNode vn) = do
  cb <- syncCallback AlwaysRetain True (putStrLn "Hello from haskell!")
  setProp ("onclick" :: String) cb vn
  return $ VNode vn

exampleNode :: VDA.VNodeAdapter
exampleNode = vns !! 0
  where (Just (R.Success vns)) = exampleNode1

exampleNode1 :: Maybe (R.Result [VDA.VNodeAdapter])
exampleNode1 = parseVNodeS exampleStringNode


exampleNode2 :: Maybe (R.Result [VDA.VNodeAdapter])
exampleNode2 = parseVNodeS exampleStringNode2


filterRes :: (VDA.VNodeAdapter, VDA.VNodeAdapter)
filterRes = (v1,v2)
  where v1 = unwrap exampleNode1
        v2 = unwrap exampleNode2
        unwrap (Just (R.Success vda)) = vda !! 0

exampleStringNode :: String
exampleStringNode = [here|
<table style="width:100%">
  <tr>
    <td>
      Jill
    <td>
      Smith
    <td>
      50
  <tr>
    <td>
      Evan
    <td>
      Jackson
    <td>
      941
|]


exampleStringNode2 :: String
exampleStringNode2 = [here|
<table style="width:100%">
  <tr>
    <td>
      Jill
    <td>
      Smith
    <td>
      50
  <tr>
    <td>
      Evan
    <td>
      Jackson
    <td>
      10000
|]
