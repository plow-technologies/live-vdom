{-# LANGUAGE OverloadedStrings #-}


module VDOM.Adapter.Types where


import Data.Int
import Data.Word
import Data.Text
import Data.Aeson

data Property = Property {
  propertyName :: String
, propertyValue :: JSProp
}


data VNodeAdapter = VNodeAdapter {
  vNodeAdapterTagName :: String
, vNodeAdapterProps :: [Property]
, vNodeAdapterChildren :: [VNodeAdapter]
}

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
            | JSPValue Value
            | JSPList [JSProp]
            | JSPMaybe (Maybe JSProp)
            | JSPT2 (JSProp, JSProp)
            | JSPT3 (JSProp, JSProp, JSProp)
            | JSPT4 (JSProp, JSProp, JSProp, JSProp)
            | JSPT5 (JSProp, JSProp, JSProp, JSProp, JSProp)
            | JSPT6 (JSProp, JSProp, JSProp, JSProp, JSProp, JSProp)

-- instance ToJSRef JSProp where
--   toJSRef (JSPText t) = toJSRef t
--   toJSRef (JSPBool b) = toJSRef b
--   toJSRef (JSPInt i) = toJSRef i
--   toJSRef (JSPInt8 i8) = toJSRef i8
--   toJSRef (JSPInt8 i16) = toJSRef i16
--   toJSRef (JSPInt8 i32) = toJSRef i32
--   toJSRef (JSPWord w) = toJSRef w
--   toJSRef (JSPWord8 w8)  = toJSRef w8
--   toJSRef (JSPWord16 w16)  = toJSRef w16
--   toJSRef (JSPWord32 w32) = toJSRef w32
--   toJSRef (JSPFloat f) = toJSRef f
--   toJSRef (JSPDouble d) = toJSRef d
--   toJSRef (JSPValue v) = toJSRef v
--   toJSRef (JSPList xs) = toJSRef xs
--   toJSRef (JSPMaybe mJSPr)  = toJSRef mJSPr
--   toJSRef (JSPT2 (a, b)) = toJSRef (a,b)
--   toJSRef (JSPT3 (a, b, c)) = toJSRef (a,b,c)
--   toJSRef (JSPT4 (a, b, c, d)) = toJSRef (a,b,c,d)
--   toJSRef (JSPT5 (a, b, c, d, e)) = toJSRef (a,b,c,d,e)
--   toJSRef (JSPT6 (a, b, c, d, e, f)) = toJSRef (a,b,c,d,e,f)


test :: VNodeAdapter
test = VNodeAdapter "h1" [] [emptyDiv,buttonTag]
  where emptyDiv = VNodeAdapter "div" [] []
        buttonTag = VNodeAdapter "button" [buttonProp] []
        buttonProp = Property "type" $ JSPText "button"


--  Should render to be like:
--  <h1>
--    <div>
--    <button type="button"
