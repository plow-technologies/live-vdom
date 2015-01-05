{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module VDOM.Adapter.Types where


import           Control.Applicative
import           Control.Monad
import           Data.Int
import           Data.Text
import           Data.Typeable

import           Instances.TH.Lift

import           Language.Haskell.TH.Syntax

-- | A javascript property like 'input=value'
-- or even 'ng-click="function()"'
data Property = Property {
  propertyName  :: String
, propertyValue :: JSProp
} deriving (Show, Eq)


instance Lift Property where
  lift (Property pName pVal) = AppE <$> (AppE (ConE propName) <$> (lift pName)) <*> (lift pVal)

propName :: Name
propName = mkVdomName "Property"
type TagName = String


-- | Intermediary type between ghcjs and haskell
-- that has  a direct corrolation to the js-vnode
-- library
data VNodeAdapter =
     VText {virtualText :: String } -- ^ Child text with  no tag name, properties, or children
   | VNode {vNodeTagName :: TagName, vNodePropsList :: [Property], vNodeChildren :: [VNodeAdapter]} -- ^ Basic tree structor for a node with children and properties
    deriving (Show, Eq, Typeable)


instance Lift VNodeAdapter where
  lift (VText st) = AppE (ConE $ mkVdomName "VText") <$> (lift st)
  lift (VNode tn pList vc) = AppE <$> (AppE <$> (AppE (ConE $ mkVdomName "VNode") <$> (lift tn)) <*> (lift pList)) <*> (lift vc)
-- | The types that are representable in javascript
-- tag values
data JSProp = JSPBool Bool
            | JSPText Text
            | JSPInt Int
            | JSPFloat Float
            | JSPDouble Double
    deriving (Show, Eq)

instance Lift JSProp where
  lift (JSPBool b) = AppE (ConE jsBoolName) <$> lift b
  lift (JSPText t) = AppE (ConE jsTextName) <$> lift t
  lift (JSPInt i) = AppE (ConE jsIntName) <$> lift i
  lift (JSPFloat f) = AppE (ConE jsFloatName) <$> lift f
  lift (JSPDouble d) = AppE(ConE jsDoubleName) <$> lift d

jsBoolName, jsTextName, jsIntName, jsFloatName, jsDoubleName :: Name
jsBoolName = mkVdomName "JSPBool"
jsTextName = mkVdomName "JSPText"
jsIntName = mkVdomName "JSPInt"
jsFloatName = mkVdomName "JSPFloat"
jsDoubleName = mkVdomName "JSPDouble"

mkVdomName :: String -> Name
mkVdomName = mkName
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
test = VNode "h1" [] [emptyDiv,buttonTag]
  where emptyDiv = VNode "div" [] []
        buttonTag = VNode "button" [buttonProp] [VText "Button Thing!"]
        buttonProp = Property "type" $ JSPText "button"

--  Should render to be like:
--  <h1>
--    <div>
--    <button type="button">Button Thing!
