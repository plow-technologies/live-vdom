{-# LANGUAGE OverloadedStrings #-}

module VDOM.Adapter.Types where


import           Data.Int
import           Data.Text
import           Data.Word

-- | A javascript property like 'input=value'
-- or even 'ng-click="function()"'
data Property = Property {
  propertyName  :: String
, propertyValue :: JSProp
} deriving (Show)

type TagName = String


-- | Intermediary type between ghcjs and haskell
-- that has  a direct corrolation to the js-vnode
-- library
data VNodeAdapter =
     VText {virtualText :: String } -- ^ Child text with  no tag name, properties, or children
   | VNode {vNodeTagName :: TagName, vNodePropsList :: [Property], vNodeChildren :: [VNodeAdapter]} -- ^ Basic tree structor for a node with children and properties
    deriving (Show)


-- | The types that are representable in javascript
-- tag values
data JSProp = JSPBool Bool
            | JSPText Text
            | JSPInt Int
            | JSPInt8 Int8
            | JSPInt16 Int16
            | JSPInt32 Int32
            | JSPWord Word
            | JSPWord8 Word8
            | JSPWord16 Word16
            | JSPWord32 Word32
            | JSPFloat Float
            | JSPDouble Double
    deriving (Show)

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
instance IsJSProp Int8 where
    toJSProp = JSPInt8
instance IsJSProp Int16 where
    toJSProp = JSPInt16
instance IsJSProp Int32 where
    toJSProp = JSPInt32
instance IsJSProp Word where
    toJSProp = JSPWord
instance IsJSProp Word8 where
    toJSProp = JSPWord8
instance IsJSProp Word16 where
    toJSProp = JSPWord16
instance IsJSProp Word32 where
    toJSProp = JSPWord32
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
