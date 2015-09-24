{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module LiveVDom.Adapter where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import qualified Data.Traversable    as TR

import           LiveVDom.Adapter.Types

import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import qualified GHCJS.VDOM as VD
import qualified GHCJS.VDOM.Element as E
import qualified Data.JSString as JSTR
import qualified JavaScript.Object as JSO
import qualified JavaScript.Object.Internal as JSO
import qualified GHCJS.VDOM.Event as EV
import GHCJS.Foreign.Callback
import GHCJS.VDOM.Unsafe
import GHCJS.VDOM.Attribute
import Unsafe.Coerce
import GHCJS.Marshal.Pure
import qualified GHCJS.Prim.Internal.Build as IB
import Data.Bifunctor

-- | The orphan instance is to seperate the GHCJS dependency
--   from the JSProp definition
--   This just pushes the data into a JSRef and then casts
--   it back to the correct type so that
--   toJSRef :: JSProp -> IO (JSRef JSProp)
instance ToJSRef JSProp where
  toJSRef (JSPText t) = toJSRef t
  toJSRef (JSPBool b) = toJSRef b
  toJSRef (JSPInt i) = toJSRef i
  toJSRef (JSPFloat f) = toJSRef f
  toJSRef (JSPDouble d) = toJSRef d

-- | Newtype to wrap the [Property] so that
newtype PropList = PropList { unPropList :: [Property]} deriving (Show)

-- | The orphan instance is again to seperate the GHCJS dependency
--   from the definition of property
instance ToJSRef PropList where
  toJSRef (PropList xs) = do
    attr@(JSO.Object attrO) <- JSO.create
    foldM_ insert attr xs 
    props@(JSO.Object propsO) <- JSO.create
    JSO.setProp (JSTR.pack "attributes") attrO props
    return $ propsO
    where
      -- VDom uses the property object like a Map from name to value
      -- So we create a map for vdom to access
      insert obj (Property name value) = do
        val <- toJSRef value
        JSO.setProp (JSTR.pack name) val obj
        return obj


toVNode :: VNodeAdapter -> IO VD.VNode
toVNode (VNode events aTagName aProps aChildren) = do
  let attrs = buildProperties aProps
      attrList = events ++ attrs
  children <- TR.mapM toVNode aChildren
  return $ E.custom tagName attrList $ mChildren children
  where tagName = JSTR.pack aTagName
        mChildren xs = mkChildren xs
toVNode (VText _ev inner) = return $ E.text $ JSTR.pack inner


buildProperties :: [Property] -> [Attribute]
buildProperties = fmap buildProperty

buildProperty :: Property -> Attribute
buildProperty (Property name (JSPBool b)) = mkAttribute (JSTR.pack name) (pCastToJSRef b)
buildProperty (Property name (JSPText t)) = mkAttribute (JSTR.pack name) (pCastToJSRef t)
buildProperty (Property name (JSPInt i)) = mkAttribute (JSTR.pack name) (pCastToJSRef i)
buildProperty (Property name (JSPFloat f)) = mkAttribute (JSTR.pack name) (pCastToJSRef f)
buildProperty (Property name (JSPDouble d)) = mkAttribute (JSTR.pack name) (pCastToJSRef d)


pCastToJSRef :: PToJSRef a => a -> JSRef
pCastToJSRef = pToJSRef

-- DOM access functions

-- attributes
getCurrentValue :: (FromJSRef b) => JSRef -> IO (Maybe b)
getCurrentValue = getValue <=< getTarget

getValue :: (FromJSRef b) => JSRef -> IO (Maybe b)
getValue ref = fromJSRef =<< JSO.unsafeGetProp "value" (JSO.Object ref)

getCurrentInnerHTML :: (FromJSRef b) => JSRef -> IO (Maybe b)
getCurrentInnerHTML = getInnerHTML <=< getTarget

getInnerHTML :: (FromJSRef b) => JSRef -> IO (Maybe b)
getInnerHTML ref = fromJSRef =<< JSO.unsafeGetProp "innerHTML" (JSO.Object ref)

getTarget :: JSRef -> IO (JSRef)
getTarget ref = JSO.unsafeGetProp "target" (JSO.Object ref)

canvasLoad :: (JSRef -> IO ()) -> Attribute
canvasLoad = undefined 

