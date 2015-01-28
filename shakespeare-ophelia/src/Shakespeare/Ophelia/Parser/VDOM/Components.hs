{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Shakespeare.Ophelia.Parser.VDOM.Components where

import           Control.Concurrent.STM.Notify
import Control.Concurrent


import           Shakespeare.Ophelia.Parser.VDOM.Event
import           Shakespeare.Ophelia.Parser.VDOM.Types
import           Shakespeare.Ophelia.QQ

import           VDOM.Adapter

import           Control.Monad

button :: Address (Event ()) -> String -> LiveVDom JSEvent
button addr text = addEvent (JSClick . void $ sendIO addr $ Fired () ) [gertrude|
  <button type="button">
    #{text}
|]

textBox :: Address (Event String) -> (Maybe String) -> LiveVDom JSEvent
textBox addr mStr = addEvent (JSInput $ \str -> void $ sendIO addr $ Fired str) tb
  where tb = case mStr of
                    Nothing -> [gertrude|<input type="text">|]
                    (Just str) -> [gertrude|
                                     <input type="text">
                                       #{str}
                                   |]