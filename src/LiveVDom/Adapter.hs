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
import           GHCJS.Foreign.QQ

import           GHCJS.Marshal
import           GHCJS.Types
import qualified GHCJS.VDOM as VD
import qualified GHCJS.VDOM.Element as E
import qualified Data.JSString as JSTR
import qualified  JavaScript.Object as JSO
import qualified JavaScript.Object.Internal as JSO
import qualified GHCJS.VDOM.Event as EV
import GHCJS.Foreign.Callback
import GHCJS.VDOM.Unsafe
import GHCJS.VDOM.Attribute
import Unsafe.Coerce
import GHCJS.Marshal.Pure
import qualified GHCJS.Prim.Internal.Build as IB
import Data.Bifunctor
import qualified  JavaScript.Object.Internal as JSOI

--ghcjs-base
import           Data.JSString

-- | The orphan instance is to seperate the GHCJS dependency
--   from the JSProp definition
--   This just pushes the data into a JSVal and then casts
--   it back to the correct type so that
--   toJSVal :: JSProp -> IO (JSVal JSProp)
instance ToJSVal JSProp where
  toJSVal (JSPString t) = toJSVal t
  toJSVal (JSPBool b) = toJSVal b
  toJSVal (JSPInt i) = toJSVal i
  toJSVal (JSPFloat f) = toJSVal f
  toJSVal (JSPDouble d) = toJSVal d

-- | Newtype to wrap the [Property] so that
newtype PropList = PropList { unPropList :: [Property]} deriving (Show)

-- | The orphan instance is again to seperate the GHCJS dependency
--   from the definition of property
instance ToJSVal PropList where
  toJSVal (PropList xs) = do
    attr@(JSO.Object attrO) <- JSO.create
    foldM_ insert attr xs 
    props@(JSO.Object propsO) <- JSO.create
    JSO.setProp (JSTR.pack "attributes") attrO props
    return $ propsO
    where
      -- VDom uses the property object like a Map from name to value
      -- So we create a map for vdom to access
      insert obj (Property name value) = do
        val <- toJSVal value
        JSO.setProp name val obj
        return obj


toVNode :: VNodeAdapter -> IO VD.VNode
toVNode (VNode events aTagName aProps aChildren) = do
  let evs = events
      attrs = mkCompleteAttributeObject $ buildProperties aProps :: Attribute
      attrList = attrs:evs
  children <- TR.mapM toVNode aChildren
  let rslt = (E.custom tagName attrList $ mChildren children)
  return rslt  
  where tagName = JSTR.pack aTagName
        mChildren xs = mkChildren xs
        mkCompleteAttributeObject = mkAttributeFromList "attributes" 
                                    
toVNode (VText _ev inner) = return $ E.text $ inner


buildProperties :: [Property] -> [Attribute]
buildProperties = fmap buildProperty

buildProperty :: Property -> Attribute
buildProperty (Property name (JSPBool b)) = mkAttribute name (pCastToJSVal b)
buildProperty (Property name (JSPString s)) = mkAttribute name (pCastToJSVal s)
buildProperty (Property name (JSPInt i)) = mkAttribute name (pCastToJSVal i)
buildProperty (Property name (JSPFloat f)) = mkAttribute name (pCastToJSVal f)
buildProperty (Property name (JSPDouble d)) = mkAttribute name (pCastToJSVal d)


pCastToJSVal :: PToJSVal a => a -> JSVal
pCastToJSVal = pToJSVal

-- DOM access functions

getCurrentValue :: (FromJSVal b) => JSVal -> IO (Maybe b)
getCurrentValue = getValue <=< getTarget

getValue :: (FromJSVal b) => JSVal -> IO (Maybe b)
getValue ref = fromJSVal =<< JSO.unsafeGetProp "value" (JSOI.Object ref)

getCurrentInnerHTML :: (FromJSVal b) => JSVal -> IO (Maybe b)
getCurrentInnerHTML = getInnerHTML <=< getTarget

getInnerHTML :: (FromJSVal b) => JSVal -> IO (Maybe b)
getInnerHTML ref = fromJSVal =<< JSO.unsafeGetProp "innerHTML" (JSOI.Object ref)

getTarget :: JSVal -> IO (JSVal)
getTarget ref = JSO.unsafeGetProp "target" (JSOI.Object ref)

canvasLoad :: (JSVal -> IO ()) -> Attribute
canvasLoad = undefined 

