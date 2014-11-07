module Shakespeare.Dynamic.Adapter where

import           Control.Applicative
import           Data.Text

import           Data.Aeson

import           VDOM.Adapter

import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.PureMarshal
import           GHCJS.Types
import           GHCJS.VDOM
import           Unsafe.Coerce       (unsafeCoerce)
import Data.Foldable
import System.IO.Unsafe (unsafePerformIO)

{-# LANGUAGE FlexibleInstances #-}

-- js_vnode :: JSString -> Properties -> Children -> VNode

instance ToJSRef JSProp where
  toJSRef (JSPText t) = castToJSRef t
  toJSRef (JSPBool b) = castToJSRef b
  toJSRef (JSPInt i) = castToJSRef i
  toJSRef (JSPInt8 i8) = castToJSRef i8
  toJSRef (JSPInt8 i16) = castToJSRef i16
  toJSRef (JSPInt8 i32) = castToJSRef i32
  toJSRef (JSPWord w) = castToJSRef w
  toJSRef (JSPWord8 w8)  = castToJSRef w8
  toJSRef (JSPWord16 w16)  = castToJSRef w16
  toJSRef (JSPWord32 w32) = castToJSRef w32
  toJSRef (JSPFloat f) = castToJSRef f
  toJSRef (JSPDouble d) = castToJSRef d

castToJSRef x = castRef <$> toJSRef x


type FuckOff = [Property]

newtype PropList = PropList { unPropList :: [Property]}
instance ToJSRef (PropList) where
  toJSRef (PropList xs) = do
    x <- newObj
    foldlM update x xs
    return x

update x (Property name value) = do
  val <- toJSRef value
  setProp name val x
  return x
-- js_vnode_conv :: (ToJSString tag) => tag

zipProperty (Property name val) = (name,val)

-- makeSingleProp (Property name val) = toJSProp' (toJSString name) val

-- toProps [] =  noProps
-- toProps [x] =  makeSingleProp x
-- toProps xs =  singleProp $ zipProperty <$> xs

toVNode :: VNodeAdapter -> VNode
toVNode (VNodeAdapter aTagName innerText aProps aChildren) = js_vnode tagName props $ mChildren aChildren
  where tagName = toJSString aTagName
        props = toProperties . castRef . unsafePerformIO . toJSRef $ PropList aProps
        mChildren [] = mkChildren [text $ toJSString innerText]
        mChildren xs = mkChildren $ (text $ toJSString innerText): (toVNode <$> aChildren)


-- ioPropertyList :: [Property] -> IO (JSRef [Property])
-- ioPropertyList pList