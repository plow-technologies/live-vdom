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
import           Control.Concurrent
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap                 as IM
import           Data.Maybe
import           Prelude                     hiding (div)

import           Data.Aeson

import           System.IO

import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Types
import           GHCJS.VDOM
import           GHCJS.VDOM.QQ

import           Shakespeare.Dynamic.Adapter

import           VDOM.Adapter

import           Control.Arrow


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
animate n r s =
  let s' = step s
      r' = render s'
      p  = diff r r'
  in  s' `seq` redraw n p >> threadDelay (2*1000*1000) >> animate n r' s' -- for async calculation, sync repaint
--  in atAnimationFrame (patch n p >> animate n r' s') -- sync all

oneFrameAnimate :: DOMNode -> VNode ->  IO ()
oneFrameAnimate n r = do
  let p = diff r exampleNode
  redraw n p
 where
   exampleNode = exNode3


printFullJSRef :: JSRef a -> IO ()
printFullJSRef r = undefined

exNode = js_vnode ( "cowboy") noProps (mkChildren [(text "powerd"  )])

exNode2 = toVNode exampleVNode

exNode3 = toVNode exampleVNode2


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
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  let s = mkState 167 101 10 20
  oneFrameAnimate root emptyDiv


exampleVNode :: VNodeAdapter
exampleVNode = VNodeAdapter "h1" "internal1" [] [emptyDiv,emptyDiv2,buttonTag "button 0", populatedDiv]
  where emptyDiv = VNodeAdapter "div" "Internal2" [idProp] []
        emptyDiv2 = VNodeAdapter "div" "" [] []
        populatedDiv = VNodeAdapter "div" "Internal 3" [idProp] [buttonTag "button 1", populatedDiv2]
        populatedDiv2 = VNodeAdapter "div" "Internal 4" [idProp] [buttonTag "Button 2"]
        buttonTag name = VNodeAdapter "button" name [alt,buttonProp, buttonId] []
        buttonProp = Property "type" $ JSPText "button"
        buttonId = Property "id" $ JSPText "abuttonid!"
        alt = Property "name" $ JSPText "AltText!"
        idProp = Property "id" $ JSPText "somethings"

exampleVNode2 :: VNodeAdapter
exampleVNode2 = VNodeAdapter "h1" "internal1" [] [button]
  where button = VNodeAdapter "button" "Click me" [bType,foo] []
        bType = Property "type" $ JSPText "button"
        foo = Property "foo" $ JSPText "bar"

failedNode :: VNodeAdapter
failedNode = VNodeAdapter "h2" "failed" [] []
-- This example should render:
 --  <h1>
 --    internal1
 --    <div>Internal2</div>
 --    <div></div>
 --    <button type="button" id="abuttonid!">Button Thing!</button>
 --  </h1>
--
