{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE  QuasiQuotes #-}
module LiveVDom.Adapter where

import           Control.Applicative
import           Control.Monad
import qualified Data.Traversable    as TR

import           LiveVDom.Adapter.Types

import           GHCJS.Foreign
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
    attr@(JSO.Object attrO) <- JSO.create
    foldM_ insert attr xs 
    props@(JSO.Object propsO) <- JSO.create
    JSO.setProp (JSTR.pack "attributes") attrO props
    return $ castRef propsO
    where
      -- VDom uses the property object like a Map from name to value
      -- So we create a map for vdom to access
      insert obj (Property name value) = do
        val <- toJSRef value
        JSO.setProp (JSTR.pack name) val obj
        return obj


-- | Convert a VNodeAdapter to a VNode in order to diff it with vdom
-- and add the event hooks
toVNode :: VNodeAdapter -> IO VD.VNode
toVNode (VNode events aTagName aProps aChildren) = do
  let evs = buildEvents events
      attrs = buildProperties aProps
      attrList = evs ++ attrs
  -- props <- unsafeToAttributes . castRef <$> (toJSRef $ PropList aProps)
  children <- TR.mapM toVNode aChildren
  return $ E.custom tagName attrList $ mChildren children
  where tagName = JSTR.pack aTagName
        mChildren xs = mkChildren xs
toVNode (VText _ev inner) = return $ E.text $ JSTR.pack inner


-- | Add a list of events to a list of properties
--   that can be added to a dom object
-- addEvents :: [JSEvent] -> Attributes' -> IO VD.Attributes'
-- addEvents events props = foldM addEvent props events
--   where addEvent pl (JSInput f)  = EV.keypress f pl
--         addEvent pl (JSKeypress f) = EV.keypress f pl
--         addEvent pl (JSClick f) = (\cb -> EV.click cb pl) <$> (mkCallback f)
--         addEvent pl (JSDoubleClick f) = (\cb -> EV.dblclick cb pl) <$> (mkCallback f)
--         addEvent pl (JSCanvasLoad f) = canvasLoad f pl
--         mkCallback = syncCallback ContinueAsync


buildProperties :: [Property] -> [Attribute]
buildProperties = fmap buildProperty

buildProperty :: Property -> Attribute
-- buildProperty (Property name (JSPBool b)) = Attribute (JSTR.pack name) $ castRef $ pToJSRef b
-- buildProperty (Property name (JSPText t)) = Attribute (JSTR.pack name) $ castRef $ pToJSRef t
-- buildProperty (Property name (JSPInt i)) = Attribute (JSTR.pack name) $ castRef $ pToJSRef i
-- buildProperty (Property name (JSPFloat f)) = Attribute (JSTR.pack name) $ castRef $ pToJSRef f
-- buildProperty (Property name (JSPDouble d)) = Attribute (JSTR.pack name) $ castRef $ pToJSRef d
buildProperty = undefined

buildEvents :: [JSEvent] -> [Attribute]
buildEvents = fmap buildEvent

buildEvent :: JSEvent -> Attribute
buildEvent (JSInput f) = EV.keypress $ \ev -> f =<< getCurrentValue (unsafeCoerce ev)
buildEvent (JSKeypress f) = EV.keypress $ \ev -> f =<< getCurrentValue (unsafeCoerce ev)
buildEvent (JSClick f) = EV.click (const f)
buildEvent (JSDoubleClick f) = EV.dblclick (const f)
buildEvent (JSCanvasLoad f) = canvasLoad f


getCurrentValue :: (FromJSRef b) => JSRef a -> IO b
getCurrentValue x = getValue =<< getTarget x

getTarget :: JSRef a -> IO (JSRef b)
getTarget = undefined

getValue :: (FromJSRef b) => JSRef a -> IO b
getValue = undefined

canvasLoad :: (JSRef a -> IO ()) -> Attribute
canvasLoad = undefined 
