module Shakespeare.Dynamic.Adapter where

import           Control.Applicative
import           Data.Text

import           VDOM.Adapter

import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           GHCJS.VDOM


-- js_vnode :: JSString -> Properties -> Children -> VNode



toJSProp' n (JSPText t) = singleProp n t
toJSProp' n (JSPBool b) = singleProp n b
toJSProp' n (JSPInt i) = singleProp n i
toJSProp' n (JSPInt8 i8) = singleProp n i8
toJSProp' n (JSPInt8 i16) = singleProp n i16
toJSProp' n (JSPInt8 i32) = singleProp n i32
toJSProp' n (JSPWord w) = singleProp n w
toJSProp' n (JSPWord8 w8) = singleProp n w8
toJSProp' n (JSPWord16 w16) = singleProp n w16
toJSProp' n (JSPWord32 w32) = singleProp n w32
toJSProp' n (JSPFloat f) = singleProp n f
toJSProp' n (JSPDouble d) = singleProp n d


-- toJSProps ::


-- js_vnode_conv :: (ToJSString tag) => tag

zipProperty (Property name val) = (name,val)

makeSingleProp (Property name val) = toJSProp' (toJSString name) val

toProps [] =  noProps
toProps [x] =  makeSingleProp x
toProps (x:xs) =  makeSingleProp x

toVNode :: VNodeAdapter -> VNode
toVNode (VNodeAdapter aTagName innerText aProps aChildren) = js_vnode tagName props $ mChildren aChildren
  where tagName = toJSString aTagName
        props = toProps aProps
        mChildren [] = mkChildren [text $ toJSString innerText]
        mChildren xs = mkChildren $ (text $ toJSString innerText): (toVNode <$> aChildren)