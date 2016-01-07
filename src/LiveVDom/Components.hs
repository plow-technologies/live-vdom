{-# LANGUAGE OverloadedStrings #-}
module LiveVDom.Components
  ( Attribute
  , button
  , buttonWith
  , buttonWithKids
  , label
  , labelWith
  , inputSubmit
  , inputSubmitWith
  , textBox
  , textBoxWith
  , passwordBoxWith
  , textAreaWith
  , formWith
  , linkWith
  , imageWith
  , numberBox
  , numberBoxWith
  , doubleBoxWith
  , selectList
  , selectListWith
  , forEach
  , forEach'
  ) where

import           Control.Concurrent.STM.Notify


import           LiveVDom.Event
import           LiveVDom.Types hiding (LiveVDom)

import           LiveVDom.Adapter
import           LiveVDom.Adapter.Types
import           LiveVDom.UserTypes

import           Control.Applicative
import           LiveVDom.Message
import           Control.Monad
import           Data.Traversable
import           Data.Text (Text, pack)
import           Text.Read
import qualified Data.Map as Map
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified GHCJS.VDOM.Event as EV
import           GHCJS.VDOM.Attribute hiding (id)

import           Control.Concurrent
import           Unsafe.Coerce
import           GHCJS.Marshal

--ghcjs-base
import           Data.JSString          (JSString)
import qualified Data.JSString          as JS (pack, unpack)


{- BUG - buildEvent
  looks like there is delay from when the event gets called and when the variable gets passed to the event
  if you add a threadDelay it causes some lag but the variable is passed. If there is no delay then the
  variable will not make it until the next event
-}

-- Events

clickToGetDivText :: (FromJSVal b) => (b -> IO()) -> Attribute
clickToGetDivText f = EV.click $ \ev -> do
  threadDelay 1
  mVal <- getCurrentInnerHTML (unsafeCoerce ev)
  case mVal of
    (Just v) -> f v
    Nothing -> return ()


inputChange :: (FromJSVal b) => (b -> IO()) -> Attribute
inputChange f = EV.change $ \ev -> do
  threadDelay 1
  mVal <- getCurrentValue (unsafeCoerce ev)
  case mVal of
    (Just v) -> f v
    Nothing -> return ()

keypress :: (FromJSVal b) => (b -> IO()) -> Attribute
keypress f = EV.keypress $ \ev -> do
  threadDelay 1
  mVal <- getCurrentValue (unsafeCoerce ev)
  case mVal of
    (Just v) -> f v
    Nothing -> return ()

keydown :: (FromJSVal b) => (b -> IO()) -> Attribute
keydown f = EV.keydown $ \ev -> do
  threadDelay 1
  mVal <- getCurrentValue (unsafeCoerce ev)
  case mVal of
    (Just v) -> f v
    Nothing -> return ()

-- Components

-- | A basic button component with the default of accepting an STM Address
button :: Address (Event ()) -> [Property] -> JSString -> LiveVDom
button addr = buttonWith (sendMessage addr $ Fired ())

-- | Add a button where you can send/receive non-blocking
-- messages with the Message monad
-- when the button is pressed
buttonWith :: Message b -> [Property] -> JSString -> LiveVDom
buttonWith f props text = (flip addProps) props $ addEvent (EV.click (const $ void $ runMessages f)) $ 
  LiveVNode [] "button" [Property "type" $ JSPString "button"] $ S.fromList [StaticText [] text]

buttonWithKids :: Message b -> [Property] -> JSString -> S.Seq LiveVDom -> LiveVDom
buttonWithKids f props text children = (flip addProps) props $ addEvent (EV.click (const $ void $ runMessages f)) $ 
  LiveVNode [] "button" [Property "type" $ JSPString "button"] $ (S.><) (S.fromList [StaticText  [] text]) children

label :: Address (Event JSString) -> [Property] -> JSString -> LiveVDom
label addr = labelWith (\str -> sendMessage addr $ Fired str) 

-- | A label with a click event
labelWith :: (JSString -> Message b) -> [Property] -> JSString -> LiveVDom
labelWith f props str = (flip addProps) props $ addEvent (clickToGetDivText $ \str -> void . runMessages $ f str) l
  where
    l = LiveVNode [] "div" [] $ S.fromList [StaticText [] str]



-- | A textbox with type="text" that updates the given address with the
-- current value of the textbox each time the textbox is updated
textBox :: Address (Event JSString) -> [Property] -> Maybe JSString -> LiveVDom
textBox addr = textBoxWith (\str -> sendMessage addr $ Fired str) 

-- | A textbox that allows you to send non-blocking messages with the Message
-- monad whenever the input changes
textBoxWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
textBoxWith f props mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "input" 
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [Property "type" $ JSPString "text"])
                   S.empty

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBox :: Address (Event Int) -> LiveVDom
numberBox addr = addEvent (inputChange $ \str -> void . Data.Traversable.sequence $ sendIO addr <$> Fired <$> (readMaybe str)) tb
  where tb = LiveVNode [] "input" [Property "type" $ JSPString "text"] S.empty
  

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBoxWith :: (Event Int -> Message b) -> LiveVDom
numberBoxWith f = addEvent (inputChange $ \str -> void . runMessages . f . maybeToEvent $ readMaybe str) tb
  where tb = LiveVNode [] "input" [Property "type" $ JSPString "number"] S.empty
        maybeToEvent (Nothing) = Unfired
        maybeToEvent (Just e) = Fired e

doubleBoxWith :: (Double -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
doubleBoxWith f props mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "input" 
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [Property "type" $ JSPString "number"])
                   S.empty


-- | A dropdown list
selectList :: (Eq v) 
           => Map.Map JSString v -- ^ Map from strings (displayed in the menu) to values
           -> (v -> Message ()) -- ^ Selection handler, given the value associated with the selected string
           -> [Property] -- ^ Extra properties for the select element
           -> Maybe v -- ^ Default selection
           -> LiveVDom
selectList kvMap messageFunc props mSelected = 
    (flip addProps) props
  $ addEvents [ (inputChange $ \str -> runMessages $ lookupKey str), (keydown $ \str -> runMessages $ lookupKey str)]
  $ LiveVNode [] "select" []
  $ fmap (\(k,v) -> option (Just v == mSelected) k) (S.fromList $ Map.toList kvMap)
  where lookupKey s = case Map.lookup s kvMap of
                        (Nothing) -> debug $ "Error looking up " ++ (show s)
                        (Just val) -> messageFunc val

-- | A dropdown list built from non-string keys and a function to derive selector names from key-value pairs
selectListWith :: (Ord k, Eq v) => ((k,v) -> JSString) -> Map.Map k v -> (v -> Message ()) -> [Property] -> Maybe v -> LiveVDom
selectListWith buildDisplay kvMap = selectList displayMap
  where displayMap = Map.fromList $ (\t@(_,v) -> (buildDisplay t, v) ) <$> Map.toList kvMap


option :: Bool -> JSString -> LiveVDom
option selected opt = LiveVNode [] "option" ((if selected then ((Property "selected" $ JSPBool True):) else id) $ [Property "value" $ JSPString opt]) $ S.fromList [StaticText [] opt]


forEach :: STMMailbox (S.Seq a) -- ^ Values to map over
          -> (a -> (Maybe a -> Message ()) -> LiveVDom) -- ^ Function to generate dom given an element and a function to change the current value
          -> STMEnvelope (S.Seq (LiveVDom))     
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


forEach' :: STMMailbox (S.Seq a) -- ^ Values to map over
          -> (a -> (Maybe a -> Message ()) -> STMEnvelope LiveVDom) -- ^ Function to generate dom given an element and a function to change the current value
          -> STMEnvelope (S.Seq LiveVDom)    
forEach' mb func = join $ T.sequence <$> (fmap buildDom) <$> withIndices
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
-- on STMEnvelope but allows for updating the current value
-- as well
withMailbox :: STMMailbox a
              -> (a -> (a -> Message ()) -> LiveVDom)
              -> STMEnvelope (LiveVDom)
withMailbox mb@(env, _) buildFunc = buildDom <$> env
  where buildDom value = buildFunc value (\newValue -> modifyMailbox mb (const newValue))


-- Components that Michael added, need to decide which ones are general enough to be used in many projects
-- which ones should be built locally

inputSubmit :: Address (Event ()) -> [Property] -> JSString -> LiveVDom
inputSubmit addr = inputSubmitWith (sendMessage addr $ Fired ())

inputSubmitWith :: Message b -> [Property] -> JSString -> LiveVDom
inputSubmitWith f props text = (flip addProps) props $ addEvent (EV.click (const $ void $ runMessages f)) $ 
  LiveVNode [] "input" [Property "type" $ JSPString "submit"] $ S.fromList [StaticText [] text]

passwordBoxWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
passwordBoxWith f props mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "input" 
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [Property "type" $ JSPString "password"])
                   S.empty

textAreaWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
textAreaWith f props mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "textarea" 
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [Property "type" $ JSPString "text"])
                   S.empty

formWith :: (JSString -> Message b) -> [Property] -> S.Seq LiveVDom -> Maybe JSString -> LiveVDom
formWith f props children mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "form" 
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [Property "type" $ JSPString "text"])
                   children

linkWith :: (JSString -> Message b) -> [Property] -> S.Seq LiveVDom -> Maybe JSString -> LiveVDom
linkWith f props children mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "a" 
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [])
                   children

imageWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
imageWith f props mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "img" 
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [])
                   S.empty

