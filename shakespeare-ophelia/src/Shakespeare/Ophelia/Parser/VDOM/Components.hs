{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Shakespeare.Ophelia.Parser.VDOM.Components where

import           Control.Concurrent.STM.Notify


import           Shakespeare.Ophelia.Parser.VDOM.Event
import           Shakespeare.Ophelia.Parser.VDOM.Types
import           Shakespeare.Ophelia.QQ

import           VDOM.Adapter

import           Control.Monad

button :: Address (Event ()) -> String -> LiveVDom JSEvent
button addr text = addEvent (JSClick $ putStrLn "Testing!") [gertrude|
  <button type="button">
    #{text}
|]

