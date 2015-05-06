{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Shakespeare.Dynamic.Components where

import           Control.Concurrent.STM.Notify


import           Shakespeare.Dynamic.Event
import           Shakespeare.Ophelia.Parser.VDOM.Types
import           Shakespeare.Ophelia.QQ

import           VDOM.Adapter

import           Control.Applicative
import           Control.Concurrent.STM.Message
import           Control.Monad
import           Data.Traversable
import           Text.Read


-- | A basic button component with the default of accepting an STM Address
button :: Address (Event ()) -> [Property] -> String -> LiveVDom JSEvent
button addr = buttonWith (sendMessage addr $ Fired ())

-- | Add a button where you can send/receive non-blocking
-- messages with the Message monad
-- when the button is pressed
buttonWith :: Message b -> [Property] -> String -> LiveVDom JSEvent
buttonWith f props text = (flip addProps) props $ addEvent (JSClick . void $ runMessages f) [gertrude|
  <button type="button">
    #{text}
|]

-- | A textbox with type="text" that updates the given address with the
-- current value of the textbox each time the textbox is updated
textBox :: Address (Event String) -> [Property] -> Maybe String -> LiveVDom JSEvent
textBox addr = textBoxWith (\str -> sendMessage addr $ Fired str) --addEvent (JSInput $ \str -> void $ sendIO addr $ Fired str) tb

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBox :: Address (Event Int) -> LiveVDom JSEvent
numberBox addr = addEvent (JSInput $ \str -> void . Data.Traversable.sequence $ sendIO addr <$> Fired <$> (readMaybe str)) tb
  where tb = [gertrude|<input type="numberBox">|]

-- | A textbox that allows you to send non-blocking messages with the Message
-- monad whenever the input changes
textBoxWith :: (String -> Message b) -> [Property] -> Maybe String -> LiveVDom JSEvent
textBoxWith f props mStr = (flip addProps) props $ addEvent (JSInput $ \str -> void . runMessages $ f str) tb
  where tb = case mStr of
                    Nothing -> [gertrude|<input type="text">|]
                    (Just str) -> [gertrude|
                                     <input type="text">
                                       #{str}
                                   |]
