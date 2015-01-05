{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-
  Straight stolt on from virtual-dom
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}


module Main where


import           Control.Applicative
import           Control.Concurrent hiding (yield)
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap                 as IM
import           Data.Maybe
import           Prelude                     hiding (div)

import           Data.Aeson

import qualified Text.Trifecta.Result as R


import           System.IO

import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Types
import           GHCJS.VDOM
import           GHCJS.VDOM.QQ

import Shakespeare.Ophelia.Parser.VDOM

import           Shakespeare.Dynamic.Adapter
import           Shakespeare.Dynamic.Render

import qualified VDOM.Adapter as VDA

import           Control.Arrow

import qualified Data.Text as T
import Data.String.Here
import Data.Functor.Identity

import Pipes.Concurrent
import Pipes
import Control.Monad (forever)
import Control.Concurrent (threadDelay)


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
  container <- [js| document.createElement('div') |] :: IO DOMNode
  [js_| document.body.appendChild(`container); |] :: IO ()
  (o,i) <- emptyOut
  forkIO $ do
    runEffect $ produceVDom >-> toOutput o
    performGC
  let getContainer = [js|document.body.childNodes[1]|]
  forever $ runEffect $ fromInput i >-> (renderDom getContainer emptyDiv)


buildPropS :: String -> T.Text -> VDA.Property
buildPropS = VDA.buildProp


addOnClick :: VNode -> IO VNode
addOnClick (VNode vn) = do
  cb <- syncCallback AlwaysRetain True (putStrLn "Hello from haskell!")
  setProp ("onclick" :: String) cb vn
  return $ VNode vn

exampleNode :: VDA.VNodeAdapter
exampleNode = a !! 0
  where (Just (R.Success a)) = exampleNode1

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