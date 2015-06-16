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
import qualified Data.Map as Map
import qualified Data.Sequence as S


-- | A basic button component with the default of accepting an STM Address
button :: Address (Event ()) -> [Property] -> String -> LiveVDom JSEvent
button addr = buttonWith (sendMessage addr $ Fired ())

-- | Add a button where you can send/receive non-blocking
-- messages with the Message monad
-- when the button is pressed
buttonWith :: Message b -> [Property] -> String -> LiveVDom JSEvent
buttonWith f props text = (flip addProps) props $ addEvent (JSClick . void $ runMessages f) [gertrude|
  <button type="button">
    #{return text}
|]

-- | A textbox with type="text" that updates the given address with the
-- current value of the textbox each time the textbox is updated
textBox :: Address (Event String) -> [Property] -> Maybe String -> LiveVDom JSEvent
textBox addr = textBoxWith (\str -> sendMessage addr $ Fired str) --addEvent (JSInput $ \str -> void $ sendIO addr $ Fired str) tb

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBox :: Address (Event Int) -> LiveVDom JSEvent
numberBox addr = addEvent (JSInput $ \str -> void . Data.Traversable.sequence $ sendIO addr <$> Fired <$> (readMaybe str)) tb
  where tb = [gertrude|<input type="number">|]

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBoxWith :: (Event Int -> Message b) -> LiveVDom JSEvent
numberBoxWith f = addEvent (JSInput $ \str -> void . runMessages . f . maybeToEvent $ readMaybe str) tb
  where tb = [gertrude|<input type="number">|]
        maybeToEvent (Nothing) = Unfired
        maybeToEvent (Just e) = Fired e

-- | A textbox that allows you to send non-blocking messages with the Message
-- monad whenever the input changes
textBoxWith :: (String -> Message b) -> [Property] -> Maybe String -> LiveVDom JSEvent
textBoxWith f props mStr = (flip addProps) props $ addEvent (JSKeypress $ \str -> void . runMessages $ f str) tb
  where tb = case mStr of
                    Nothing -> [gertrude|<input type="text">|]
                    (Just str) -> [gertrude|
                                     <input type="text">
                                       #{return str}
                                   |]


selectList :: (Eq v) => Map.Map String v -> (v -> Message ()) -> [Property] -> Maybe v -> LiveVDom JSEvent
selectList kvMap messageFunc props Nothing = (flip addProps) props $ addEvent (JSInput $ \str -> runMessages $ lookupKey str) [gertrude|
<select>
  &{return $ fmap ((option False) . fst) (S.fromList $ Map.toList kvMap)}
|]
  where lookupKey s = case Map.lookup s kvMap of
                        (Nothing) -> debug $ "Error looking up " ++ (show s)
                        (Just val) -> messageFunc val
selectList kvMap messageFunc props (Just selected) = (flip addProps) props $ addEvent (JSInput $ \str -> runMessages $ lookupKey str) [gertrude|
<select>
  &{return $ fmap (\(k,v) -> option (v == selected) k) (S.fromList $ Map.toList kvMap)}
|]
  where lookupKey s = case Map.lookup s kvMap of
                        (Nothing) -> debug $ "Error looking up " ++ (show s)
                        (Just val) -> messageFunc val


selectListWith :: (Ord k, Eq v) => ((k,v) -> String) -> Map.Map k v -> (v -> Message ()) -> [Property] -> Maybe v -> LiveVDom JSEvent
selectListWith buildDisplay kvMap = selectList displayMap
  where displayMap = Map.fromList $ (\t@(_,v) -> (buildDisplay t, v) ) <$> Map.toList kvMap


option :: Bool -> String -> LiveVDom JSEvent
option False opt = [gertrude|
<option>
  #{return opt}
|]
option True opt = [gertrude|
<option selected="true">
  #{return opt}
|]


forEach :: STMMailbox (S.Seq a) -- ^ Values to map over
          -> (a -> (Maybe a -> Message ()) -> LiveVDom b) -- ^ Function to generate dom given an element and a function to change the current value
          -> STMEnvelope (S.Seq (LiveVDom b))     
forEach mb func = (fmap buildDom) <$> withIndeces
  where withIndeces = S.zip <$> stmIndexList <*> env
        stmIndexList = (increasingSeq . S.length) <$> env
        increasingSeq = S.fromList . ((flip take) [0,1..])
        buildDom (i, val) = func val (updateValue i)
        updateValue i (Just newVal) = modifyMailbox mb (S.update i newVal)
        updateValue i _ = modifyMailbox mb (remove i)
        env = fst mb
        remove i ts = appendL  $ S.viewl <$> S.splitAt i ts
        appendL (xs,(_ S.:< ys)) = xs S.>< ys
        appendL (xs,_) = xs

-- | A little wrapper around the applicative instance
-- on STMEnvelope but allows for updating the current value
-- as well
withMailbox :: STMMailbox a
              -> (a -> (a -> Message ()) -> LiveVDom b)
              -> STMEnvelope (LiveVDom b)
withMailbox mb@(env, _) buildFunc = buildDom <$> env
  where buildDom value = buildFunc value (\newValue -> modifyMailbox mb (const newValue))
