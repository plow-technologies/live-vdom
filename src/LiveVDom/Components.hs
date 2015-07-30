{-# LANGUAGE OverloadedStrings #-}
module LiveVDom.Components
  (
    button
  , buttonWith
  , textBox
  , textBoxWith
  , numberBox
  , numberBoxWith
  , selectList
  , selectListWith
  , forEach
  ) where

import           Control.Concurrent.STM.Notify


import           LiveVDom.Event
import           LiveVDom.Types hiding (LiveVDom)

import           LiveVDom.Adapter.Types
import           LiveVDom.UserTypes

import           Control.Applicative
import           LiveVDom.Message
import           Control.Monad
import           Data.Traversable
import           Data.Text (pack)
import           Text.Read
import qualified Data.Map as Map
import qualified Data.Sequence as S


-- class HasBox a where
--   box :: Address (Event a) -> [Property] -> Maybe a -> LiveVDom JSEvent
--   boxWith :: (Event a -> Message b) -> [Property] -> LiveVDom JSEvent

-- instance HasBox String where
--   box = 

-- | A basic button component with the default of accepting an STM Address
button :: Address (Event ()) -> [Property] -> String -> LiveVDom
button addr = buttonWith (sendMessage addr $ Fired ())

-- | Add a button where you can send/receive non-blocking
-- messages with the Message monad
-- when the button is pressed
buttonWith :: Message b -> [Property] -> String -> LiveVDom
buttonWith f props text = (flip addProps) props $ addEvent (JSClick . void $ runMessages f) $ 
  LiveVNode [] "button" [Property "type" $ JSPText "button"] $ S.fromList [LiveVText [] $ return text]

-- | A textbox with type="text" that updates the given address with the
-- current value of the textbox each time the textbox is updated
textBox :: Address (Event String) -> [Property] -> Maybe String -> LiveVDom
textBox addr = textBoxWith (\str -> sendMessage addr $ Fired str) --addEvent (JSInput $ \str -> void $ sendIO addr $ Fired str) tb

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBox :: Address (Event Int) -> LiveVDom
numberBox addr = addEvent (JSInput $ \str -> void . Data.Traversable.sequence $ sendIO addr <$> Fired <$> (readMaybe str)) tb
  where tb = LiveVNode [] "input" [Property "type" $ JSPText "text"] S.empty
  

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBoxWith :: (Event Int -> Message b) -> LiveVDom
numberBoxWith f = addEvent (JSInput $ \str -> void . runMessages . f . maybeToEvent $ readMaybe str) tb
  where tb = LiveVNode [] "input" [Property "type" $ JSPText "number"] S.empty
        maybeToEvent (Nothing) = Unfired
        maybeToEvent (Just e) = Fired e

-- | A textbox that allows you to send non-blocking messages with the Message
-- monad whenever the input changes
textBoxWith :: (String -> Message b) -> [Property] -> Maybe String -> LiveVDom
textBoxWith f props mStr = (flip addProps) props $ addEvent (JSKeypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "input" 
                   (maybe id ((:) . Property "value" . JSPText . pack) mStr
                     [Property "type" $ JSPText "text"])
                   S.empty

-- | A dropdown list
selectList :: (Eq v) 
           => Map.Map String v -- ^ Map from strings (displayed in the menu) to values
           -> (v -> Message ()) -- ^ Selection handler, given the value associated with the selected string
           -> [Property] -- ^ Extra properties for the select element
           -> Maybe v -- ^ Default selection
           -> LiveVDom
selectList kvMap messageFunc props mSelected = 
    (flip addProps) props
  $ addEvent (JSInput $ \str -> runMessages $ lookupKey str)
  $ LiveVNode [] "select" []
  $ fmap (\(k,v) -> option (Just v == mSelected) k) (S.fromList $ Map.toList kvMap)
  where lookupKey s = case Map.lookup s kvMap of
                        (Nothing) -> debug $ "Error looking up " ++ (show s)
                        (Just val) -> messageFunc val

-- | A dropdown list built from non-string keys and a function to derive selector names from key-value pairs
selectListWith :: (Ord k, Eq v) => ((k,v) -> String) -> Map.Map k v -> (v -> Message ()) -> [Property] -> Maybe v -> LiveVDom
selectListWith buildDisplay kvMap = selectList displayMap
  where displayMap = Map.fromList $ (\t@(_,v) -> (buildDisplay t, v) ) <$> Map.toList kvMap


option :: Bool -> String -> LiveVDom
option selected opt = LiveVNode [] "option" ((if selected then ((Property "selected" $ JSPBool True):) else id) $ [Property "value" $ JSPText $ pack opt]) $ S.fromList [LiveVText [] $ return opt]


forEach :: Mailbox (S.Seq a) -- ^ Values to map over
          -> (a -> (Maybe a -> Message ()) -> LiveVDom) -- ^ Function to generate dom given an element and a function to change the current value
          -> Envelope (S.Seq (LiveVDom))     
forEach mb func = (fmap buildDom) <$> withIndices
  where withIndices = S.zip <$> stmIndexList <*> env
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
-- on Envelope but allows for updating the current value
-- as well
withMailbox :: Mailbox a
              -> (a -> (a -> Message ()) -> LiveVDom)
              -> Envelope (LiveVDom)
withMailbox mb@(env, _) buildFunc = buildDom <$> env
  where buildDom value = buildFunc value (\newValue -> modifyMailbox mb (const newValue))
