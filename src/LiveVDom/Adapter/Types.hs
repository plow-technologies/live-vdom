{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}


module LiveVDom.Adapter.Types where


import           Control.Applicative
import           Data.Text
import           Data.Typeable

import           Instances.TH.Lift
import           Language.Haskell.TH.Syntax

import           GHCJS.Marshal
import           GHCJS.Types

import           GHCJS.VDOM.Attribute
import qualified GHCJS.VDOM.Event           as EV

--ghcjs-base
import           Data.JSString              (JSString)
import qualified Data.JSString              as JS (pack, unpack)

-- | A javascript property like 'input=value'
-- or even 'ng-click="function()"'
data Property = Property {
  propertyName  :: JSString
, propertyValue :: JSProp
} deriving (Show, Eq)


instance Lift Property where
  lift (Property pName pVal) = AppE <$> (AppE (ConE 'Property) <$> (lift . JS.unpack $ pName)) <*> (lift pVal)


type TagName = String

data JSProp = JSPBool Bool
            | JSPString JSString
            | JSPInt Int
            | JSPFloat Float
            | JSPDouble Double
    deriving (Show, Eq)

instance Lift JSProp where
  lift (JSPBool b) = AppE (ConE 'JSPBool) <$> lift b
  lift (JSPString t) = AppE (ConE 'JSPString) <$> (lift . JS.unpack $ t)
  lift (JSPInt i) = AppE (ConE 'JSPInt) <$> lift i
  lift (JSPFloat f) = AppE (ConE 'JSPFloat) <$> lift f
  lift (JSPDouble d) = AppE(ConE 'JSPDouble) <$> lift d


-- | A provisional class to make building a JSProp
-- easier
class IsJSProp a where
    toJSProp :: a -> JSProp


instance IsJSProp Bool where
    toJSProp = JSPBool

instance IsJSProp Int where
    toJSProp = JSPInt

instance IsJSProp Float where
    toJSProp = JSPFloat

instance IsJSProp Double where
    toJSProp = JSPDouble

instance IsJSProp JSString where
    toJSProp = JSPString


-- | Build a property from a name and value
buildProp :: IsJSProp a =>
             JSString       -- ^ Property name
             -> a         -- ^ Property value
             -> Property
buildProp name prop = Property name $ toJSProp prop
