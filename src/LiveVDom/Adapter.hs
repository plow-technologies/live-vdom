{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module LiveVDom.Adapter where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import qualified Data.Traversable              as TR

import           LiveVDom.Adapter.Types

import           GHCJS.Foreign
import           GHCJS.Foreign.QQ

import           Data.Bifunctor
import qualified Data.JSString                 as JSTR
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import qualified GHCJS.Prim.Internal.Build     as IB
import           GHCJS.Types
import qualified GHCJS.VDOM                    as VD
import           GHCJS.VDOM.Attribute
import qualified GHCJS.VDOM.Element            as E
import qualified GHCJS.VDOM.Event              as EV
import           GHCJS.VDOM.Unsafe
import qualified JavaScript.Object             as JSO
import qualified JavaScript.Object.Internal    as JSO
import qualified JavaScript.Object.Internal    as JSOI
import           Unsafe.Coerce

--ghcjs-base
import           Control.Concurrent.STM.Notify
import           Data.Foldable
import           Data.JSString
import           Data.List                     as L
import           Data.Monoid                   (mconcat)
import qualified Data.Sequence                 as S
import           LiveVDom.Types

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


mkVNode :: LiveVDom Attribute -> IO [VD.VNode]
mkVNode (LiveVText ev !t) = ((:[]) . E.text) <$> recvIO t
mkVNode (StaticText ev !t) = return [E.text t]
mkVNode (LiveVNode ev !tname !namespace !propsList !children) = do
  !children' <- Data.Foldable.msum <$> traverse mkVNode children :: IO [VD.VNode]
  let attrs = mkAttributeFromList "attributes" $ buildProperties propsList
      attrList = attrs:ev
      customAndNamespace = maybe E.custom (const E.customSVG) $ namespace
  return . (:[]) $ customAndNamespace (JSTR.pack tname) attrList $ mkChildren $ children'
mkVNode (LiveChild ev !ivc) = do
  !vc <- recvIO ivc
  mkVNode $ addEvents ev vc
mkVNode (LiveChildren ev !lvc) = do
  !vcs <- recvIO lvc
  Data.Foldable.msum <$> traverse (mkVNode . (addEvents ev)) vcs

-- | Print out the dom
debugDom :: LiveVDom Attribute -> IO [String]
debugDom (LiveVText ev t) =  (\i -> ["{ \"VText\": " ++ (show i) ++ " }"]) <$> recvIO t
debugDom (StaticText ev t) = return $ ["{ \"Text\": " ++ (show t) ++ " }"]
debugDom (LiveVNode ev tname namespace propsList children) = do
  children' <- Data.Foldable.concat <$> traverse debugDom (Data.Foldable.toList children) :: IO [String]
  let attrs = ""
  return $ ["{ \"VNode\": " ++ (show tname) ++ ", \"VChildren\": [" ++ (L.intercalate ", " children') ++ "] }"]
debugDom (LiveChild ev !ivc) = do
  vc <- debugDom =<< recvIO ivc :: IO [String]
  return ["{ \"LiveChild\": " ++ (vc !! 0) ++ " }"]
debugDom (LiveChildren ev lvc) = do
  vcs <- recvIO lvc
  res <- Data.Foldable.concat <$> traverse debugDom vcs :: IO [String]
  return ["{ \"LiveChildren\": [" ++ (L.intercalate ", " res) ++ "] }"]


buildProperties :: [Property] -> [Attribute]
buildProperties = fmap buildProperty

buildProperty :: Property -> Attribute
buildProperty (Property !name (JSPBool !b)) = mkAttribute name (pCastToJSVal b)
buildProperty (Property !name (JSPString !s)) = mkAttribute name (pCastToJSVal s)
buildProperty (Property !name (JSPInt !i)) = mkAttribute name (pCastToJSVal i)
buildProperty (Property !name (JSPFloat !f)) = mkAttribute name (pCastToJSVal f)
buildProperty (Property !name (JSPDouble !d)) = mkAttribute name (pCastToJSVal d)


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


-- unfortunately needs to be fixed
canvasLoad :: (JSVal -> IO ()) -> Attribute
canvasLoad = error "canvas not loaded"

