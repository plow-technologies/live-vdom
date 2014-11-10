{-# LANGUAGE OverloadedStrings #-}


module VDOM.Adapter.Types where


import Data.Int
import Data.Word
import Data.Text

data Property = Property {
  propertyName :: String
, propertyValue :: JSProp
} deriving (Show)


data VNodeAdapter = VNodeAdapter {
  vNodeAdapterTagName :: String
, vNodeAdapterInnerText :: String
, vNodeAdapterProps :: [Property]
, vNodeAdapterChildren :: [VNodeAdapter]
} deriving (Show)

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


test :: VNodeAdapter
test = VNodeAdapter "h1" "" [] [emptyDiv,buttonTag] 
  where emptyDiv = VNodeAdapter "div" "" [] []
        buttonTag = VNodeAdapter "button" "Button Thing!" [buttonProp] []
        buttonProp = Property "type" $ JSPText "button"


--  Should render to be like:
--  <h1>
--    <div>
--    <button type="button">Button Thing!
