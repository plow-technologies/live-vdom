{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ExistentialQuantification #-}


module LiveVDom.Adapter.Types where


import           Control.Applicative
import           Data.Text
import           Data.Typeable

import           Language.Haskell.TH.Syntax
import           Instances.TH.Lift

import           GHCJS.Types
import           GHCJS.Marshal

import qualified GHCJS.VDOM.Event as EV
import           GHCJS.VDOM.Attribute

-- | A javascript property like 'input=value'
-- or even 'ng-click="function()"'
data Property = Property {
  propertyName  :: String
, propertyValue :: JSProp
} deriving (Show, Eq)


instance Lift Property where
  lift (Property pName pVal) = AppE <$> (AppE (ConE 'Property) <$> (lift pName)) <*> (lift pVal)


type TagName = String


data JSEvent = JSInput       (String -> IO ())
             | JSKeydown     (String -> IO ())
             | JSKeypress    (String -> IO ())
             | JSClickWithId (String -> IO ())
             | JSClick       (IO ())
             | JSDoubleClick (IO ())
             | JSCanvasLoad  (JSRef  -> IO ())

{-
String -> IO ()
IO ()
JSRef -> IO ()
-}

-- | Intermediary type between ghcjs and haskell
-- that has  a direct corrolation to the js-vnode
-- library
data VNodeAdapter =
     VText {virtualTextEvents :: [Attribute], virtualText :: String } -- ^ Child text with  no tag name, properties, or children
   | VNode {vNodeEvents :: [Attribute], vNodeTagName :: TagName, vNodePropsList :: [Property], vNodeChildren :: [VNodeAdapter]} -- ^ Basic tree structor for a node with children and properties
    deriving (Typeable)

-- testing here
data VNodeAdapterTest =
  VNodeTest {vNodeEventsTest :: [Attribute], vNodeTagNameTest :: TagName, vNodePropsListTest :: [Property], vNodeChildrenTest :: [VNodeAdapterTest]}
-- | The types that are representable in javascript
-- tag values
data JSProp = JSPBool Bool
            | JSPText Text
            | JSPInt Int
            | JSPFloat Float
            | JSPDouble Double
    deriving (Show, Eq)

instance Lift JSProp where
  lift (JSPBool b) = AppE (ConE 'JSPBool) <$> lift b
  lift (JSPText t) = AppE (ConE 'JSPText) <$> lift t
  lift (JSPInt i) = AppE (ConE 'JSPInt) <$> lift i
  lift (JSPFloat f) = AppE (ConE 'JSPFloat) <$> lift f
  lift (JSPDouble d) = AppE(ConE 'JSPDouble) <$> lift d


-- | A provisional class to make building a JSProp
-- easier

class IsJSProp a where
    toJSProp :: a -> JSProp

instance IsJSProp Bool where
    toJSProp = JSPBool
instance IsJSProp Text where
    toJSProp = JSPText
instance IsJSProp Int where
    toJSProp = JSPInt
instance IsJSProp Float where
    toJSProp = JSPFloat
instance IsJSProp Double where
    toJSProp = JSPDouble


-- | Build a property from a name and value
buildProp :: IsJSProp a =>
             String       -- ^ Property name
             -> a         -- ^ Property value
             -> Property
buildProp name prop = Property name $ toJSProp prop


test :: VNodeAdapter
test = VNode [] "h1" [] [emptyDiv,buttonTag]
  where emptyDiv = VNode [] "div" [] []
        buttonTag = VNode [] "button" [buttonProp] [VText [] "Button Thing!"]
        buttonProp = Property "type" $ JSPText "button"

test2 :: VNodeAdapterTest
test2 = VNodeTest [EV.click (const $ print "asdf")] "div" [] []
{-
Ev.click (const increment)

EV.click (const f)
JSClick . void $ runMessages f
labelWith :: (String -> Message b) -> [Property] -> String -> LiveVDom
labelWith f props str = (flip addProps) props $ addEvent (JSClickWithId $ \str -> void . runMessages $ f str) l
  where
    l = LiveVNode [] "div" [] $ S.fromList [LiveVText [] $ return str]

data VNodeAdapterTest =
  VNodeTest {vNodeEventsTest :: [EV.MouseEvent], vNodeTagNameTest :: TagName, vNodePropsListTest :: [Property], vNodeChildrenTest :: [VNodeAdapterTest]}
-}

--  Should render to be like:
--  <h1>
--    <div>
--    <button type="button">Button Thing!
