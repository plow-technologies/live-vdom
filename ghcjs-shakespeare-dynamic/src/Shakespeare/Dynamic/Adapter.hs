{-# OPTIONS_GHC -fno-warn-orphans #-}
module Shakespeare.Dynamic.Adapter where

import           Control.Applicative
import           Control.Monad
import qualified Data.Traversable    as TR

import           VDOM.Adapter

import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import qualified GHCJS.VDOM as VD



-- | The orphan instance is to seperate the GHCJS dependency
--   from the JSProp definition
--   This just pushes the data into a JSRef and then casts
--   it back to the correct type so that
--   toJSRef :: JSProp -> IO (JSRef JSProp)
instance ToJSRef JSProp where
  toJSRef (JSPText t) = castToJSRef t
  toJSRef (JSPBool b) = castToJSRef b
  toJSRef (JSPInt i) = castToJSRef i
  toJSRef (JSPFloat f) = castToJSRef f
  toJSRef (JSPDouble d) = castToJSRef d


-- | Push a piece of data into a JSRef and cast it
castToJSRef :: ToJSRef a => a -> IO (JSRef b)
castToJSRef x = castRef <$> toJSRef x



-- | Newtype to wrap the [Property] so that
newtype PropList = PropList { unPropList :: [Property]} deriving (Show)


-- | The orphan instance is again to seperate the GHCJS dependency
--   from the definition of property
instance ToJSRef PropList where
  toJSRef (PropList xs) = do
    attr <- newObj
    foldM_ insert attr xs 
    props <- newObj
    setProp "attributes" attr props
    return props
    where
      -- VDom uses the property object like a Map from name to value
      -- So we create a map for vdom to access
      insert obj (Property name value) = do
        val <- toJSRef value
        setProp name val obj
        return obj


-- | Convert a VNodeAdapter to a VNode in order to diff it with vdom
toVNode :: VNodeAdapter -> IO VD.VNode
toVNode (VNode aTagName aProps aChildren) = do
  props <- VD.toProperties . castRef <$> (toJSRef $ PropList aProps)
  children <- TR.mapM toVNode aChildren
  return $ VD.js_vnode tagName props $ mChildren children
  where tagName = toJSString aTagName
        mChildren [] = VD.noChildren
        mChildren xs = VD.mkChildren xs
toVNode (VText inner) = return $ VD.text $ toJSString inner

