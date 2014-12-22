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

import qualified VDOM.Adapter as VDA

import           Control.Arrow

import qualified Data.Text as T
import Data.String.Here
import Data.Functor.Identity

import Pipes.Concurrent
import Pipes
import Control.Monad (forever)
import Control.Concurrent (threadDelay)


red :: JSString
red = "pixel-red"

white :: JSString
white = "pixel-white"

type Pixels = IntMap (IntMap JSString)

setPixel :: Int -> Int -> JSString -> Pixels -> Pixels
setPixel x y c p =
  let r  = p IM.! y
      r' = IM.insert x c r
  in  r' `seq` IM.insert y r' p


produceVDom :: Producer VDA.VNodeAdapter IO r
produceVDom = forever $ do
  yield v1
  lift $ threadDelay 2000000
  yield v2
  lift $ threadDelay 2000000
  where (v1,v2) = filterRes


emptyOut :: IO (Output VDA.VNodeAdapter, Input VDA.VNodeAdapter)
emptyOut = spawn Unbounded



renderDom :: IO DOMNode -> VNode -> Consumer VDA.VNodeAdapter IO ()
renderDom getContainer initial = do
  vna <- await
  newNode <- liftIO $ toVNode vna
  let p = diff initial newNode
      unv = unVNode newNode
  liftIO $ do
    print vna
    [js|console.log(`unv)|] :: IO ()
    putStrLn "redrawing"
    root' <- getContainer
    newNode `seq` redraw root' p
  renderDom getContainer newNode


data State = State { x  :: !Int, y  :: !Int
                   , dx :: !Int, dy :: !Int
                   , w  :: !Int, h  :: !Int
                   , pixels :: !Pixels
                   }

mkState :: Int -> Int -> Int -> Int -> State
mkState w h x y = State x y 1 1 w h pix
  where
    pix     = IM.fromList $ map row [0..h-1]
    row n   = (n, IM.fromList (map (col n) [0..w-1]))
    col n m = (m, if (m,n)==(x,y) then red else white)

step :: State -> State
step (State x y dx dy w h p) =
  let dx' = if x==0 then 1 else if x==(w-1) then -1 else dx
      dy' = if y==0 then 1 else if y==(h-1) then -1 else dy
      x'  = x+dx'
      y'  = y+dy'
      p'  = setPixel x' y' red (setPixel x y white p)
   in State x' y' dx' dy' w h p'

cls :: JSString -> Properties
cls name = [pr| className: name |]

render :: State -> VNode
render s = div (cls "state") [ch|pixelDiv,numDiv|]
    where
      xd       = textDiv (y s)
      yd       = textDiv (x s)
      numDiv   = div (cls "numeric") [ch|xd,yd|]
      pixelDiv = div (cls "pixels") . mkChildren $
          map (renderRowM (w s) . (pixels s IM.!)) [0..h s-1]

textDiv :: Show a => a -> VNode
textDiv x = div noProps [ch|c|]
  where
    c = text . toJSString . show $ x

renderRowM = memo renderRow

renderRow :: Int -> IntMap JSString -> VNode
renderRow w r =
  div [pr|className: 'row' |] . mkChildren $
    map (renderPixelM r) [0..w-1]

renderPixelM = memo renderPixel

renderPixel :: IntMap JSString -> Int -> VNode
renderPixel r c = div (cls (r IM.! c)) noChildren

animate :: DOMNode -> VNode -> State -> IO ()
animate n r s = do
  let s' = step s
      r' = render s'
      p  = diff r r'
      unv = unVNode r' :: JSRef ()
  [js|console.log(`unv)|] :: IO ()
  s' `seq` redraw n p
  threadDelay (2*1000*1000)
  animate n r' s' -- for async calculation, sync repaint
--  in atAnimationFrame (patch n p >> animate n r' s') -- sync all

oneFrameAnimate :: DOMNode -> VNode -> VDA.VNodeAdapter ->  IO ()
oneFrameAnimate n r v = do
  newNode <- toVNode v
  let unv = unVNode newNode
  [js|console.log(`unv)|] :: IO ()
  let p = diff r newNode
  newNode `seq` (redraw n p)
   


printFullJSRef :: JSRef a -> IO ()
printFullJSRef r = undefined

exNode = js_vnode ( "cowboy") noProps (mkChildren [(text "powerd"  )])

exNode2 = toVNode exampleVNode

exNode3 =  toVNode exampleVNode2


redraw :: DOMNode -> Patch -> IO ()
redraw node p = p `seq` atAnimationFrame (patch node p)

atAnimationFrame :: IO () -> IO ()
atAnimationFrame m = do
  cb <- fixIO $ \cb ->
    syncCallback AlwaysRetain False (release cb >> m)
  [js_| window.requestAnimationFrame(`cb); |]




-- | Original main
-- main :: IO ()
-- main = do
--   root <- [js| document.createElement('div') |]
--   [js_| document.body.appendChild(`root); |]
--   let s = mkState 167 101 10 20
--   animate root emptyDiv s




main :: IO ()
main = do
  container <- [js| document.createElement('div') |] :: IO DOMNode
  [js_| document.body.appendChild(`container); |] :: IO ()
  let s = mkState 167 101 10 20
  (o,i) <- emptyOut
  forkIO $ do
    runEffect $ produceVDom >-> toOutput o
    performGC
  let getContainer = [js|document.body.childNodes[1]|]
  forever $ runEffect $ fromInput i >-> (renderDom getContainer emptyDiv)




-- main :: IO ()
-- main = do
--   root <- [js| document.createElement('div') |]
--   [js_| document.body.appendChild(`root); |]
--   let (v1,v2) = filterRes
--   forever $ do
--     oneFrameAnimate root emptyDiv v1
--     putStrLn "1"
--     threadDelay 2000000
--     oneFrameAnimate root emptyDiv v2
--     threadDelay 2000000




exampleVNode :: VDA.VNodeAdapter
exampleVNode = VDA.VNode "h1"  [] [VDA.VText "Internal", emptyDiv,emptyDiv2,buildButton "button", populatedDiv]
  where emptyDiv = VDA.VNode "div"  [idProp] [VDA.VText "Internal 2"]
        emptyDiv2 = VDA.VNode "div" [] []
        populatedDiv = VDA.VNode "div" [idProp] [VDA.VText "Internal 3", buildButton "button 1", populatedDiv2]
        populatedDiv2 = VDA.VNode "div" [idProp] [VDA.VText "Internal 4", buildButton "Button 2"]
        buildButton name = VDA.VNode "button" [ alt,buttonProp, buttonId] [VDA.VText name]
        buttonProp = buildPropS "type" "button"
        buttonId = buildPropS "id" "abuttonid!"
        alt = buildPropS "name" "AltText!"
        idProp = buildPropS "id" "somethings"


buildPropS :: String -> T.Text -> VDA.Property
buildPropS = VDA.buildProp


addOnClick :: VNode -> IO VNode
addOnClick (VNode vn) = do
  cb <- syncCallback AlwaysRetain True (putStrLn "Hello from haskell!")
  setProp ("onclick" :: String) cb vn
  return $ VNode vn

exampleVNode2 :: VDA.VNodeAdapter
exampleVNode2 = VDA.VNode "h1"  [] [VDA.VText "button",button]
  where button = VDA.VNode "button"  [bType,foo] [VDA.VText "Click me"]
        bType = buildPropS "type" "button"
        foo = buildPropS "foo" "bar"

failedNode :: VDA.VNodeAdapter
failedNode = VDA.VNode "failed" [] [VDA.VText "h2"]
-- This example should render:
 --  <h1>
 --    internal1
 --    <div>Internal2</div>
 --    <div></div>
 --    <button type="button" id="abuttonid!">Button Thing!</button>
 --  </h1>
--

foreign import javascript unsafe "abc = $1" js_JSFunListener :: VNode -> JSString -> (JSFun (IO ())) -> IO ()

jsFunListener :: VNode -> String -> (JSFun (IO ())) -> IO ()
jsFunListener el et cb = do
    js_JSFunListener el (toJSString et) cb

printFromHaskell = asyncCallback AlwaysRetain (putStrLn "Hello from Haskell!")



-- printVEvent :: IO ()
-- printVEvent = do
--   cb <- printFromHaskell
--   let v = vEvent "on-click" cb
--   jsLog $ unVEvent v

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