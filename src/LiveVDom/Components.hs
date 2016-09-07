{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}

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
  , textBoxWithEnterKey
  , inputWith
  , passwordBoxWith
  , textAreaWith
  , formWith
  , linkWith
  , imageWith
  , numberBox
  , numberBoxWith
  , doubleBoxWith
  , selectList
  , selectList1
  , selectListWith
  , forEach
  , spanWith
  , withMailbox
  , scrollBoxWith
  ) where

import           Control.Concurrent.STM.Notify


import           LiveVDom.Event
import           LiveVDom.Types                hiding (LiveVDom)

import           LiveVDom.Adapter
import           LiveVDom.Adapter.Types
import           LiveVDom.UserTypes

import           Control.Applicative
import           Control.Monad
import qualified Data.Map                      as Map
import qualified Data.Sequence                 as S
import           Data.Text                     (Text, pack)
import           Data.Traversable
import qualified Data.Traversable              as T
import           GHCJS.VDOM.Attribute          hiding (id)
import qualified GHCJS.VDOM.Event              as EV
import           LiveVDom.Message
import           Text.Read

import           GHCJS.Foreign.QQ
import           GHCJS.Types
import qualified Data.JSString                 as JS (unpack)

import           Control.Concurrent
import           GHCJS.Marshal
import           Unsafe.Coerce

--ghcjs-base
import           Data.JSString                 (JSString)
import qualified Data.JSString                 as JS (pack, unpack)



{- BUG - buildEvent
  looks like there is delay from when the event gets called and when the variable gets passed to the event
  if you add a threadDelay it causes some lag but the variable is passed. If there is no delay then the
  variable will not make it until the next event
-}

-- Events

clickToGetDivText :: (FromJSVal b) => (b -> IO()) -> Attribute
clickToGetDivText f = EV.click $ \ev -> do
  mVal <- getCurrentInnerHTML $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing -> return ()


inputChange :: (FromJSVal b) => (b -> IO()) -> Attribute
inputChange f = EV.change $ \ev -> do
  mVal <- getCurrentValue $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing -> putStrLn "input fail"

input :: (FromJSVal b) => (b -> IO()) -> Attribute
input f = EV.input $ \ev -> do
  mVal <- getCurrentValue $ unsafeCoerce ev
  case mVal of 
   (Just v) -> f v
   Nothing -> putStrLn "input fail"


keypress :: (FromJSVal b) => (b -> IO()) -> Attribute
keypress f = EV.keypress $ \ev -> do
  mVal <- getCurrentValue $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing -> putStrLn "Keypress fail"

-- | Similar to keypress, but takes an addition function param which gets called instead if the enter key is pressed
keypressCheckEnter :: (FromJSVal b) => (b -> IO()) -> IO () -> Attribute
keypressCheckEnter f enterF = EV.keypress $ \ev -> do
  let jsVal :: JSVal
      jsVal = unsafeCoerce ev
  mVal <- getCurrentValue jsVal
  case mVal of
    Nothing -> putStrLn "enterPress fail"
    Just v -> do
      case checkForEnterKey jsVal of
        False -> f v
        True  -> enterF

-- | Returns true if the jsval (keyboard event) has keyCode 13 (Enter Key)
checkForEnterKey :: JSVal -> Bool
checkForEnterKey keyEv = [js'|(`keyEv.which == 13) || (`keyEv.keyCode == 13)|]

keydown :: (FromJSVal b) => (b -> IO()) -> Attribute
keydown f = EV.keydown $ \ev -> do
  mVal <- getCurrentValue $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing -> putStrLn "keydown fail"

keyup :: (FromJSVal b) => (b -> IO ()) -> Attribute
keyup f = EV.keyup $ \ev -> do
  mVal <- getCurrentValue $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing -> putStrLn "keyup fail"

scrollCheck :: JSString -> IO () -> Attribute
scrollCheck elementId infiniteScrollFunction = EV.scroll $ \_ -> do
  isBottom <- checkScroll elementId
  when isBottom infiniteScrollFunction

foreign import javascript unsafe "(function () {\
\    var element = document.getElementById($1);\
\    if (element) {\
\      if (element.scrollHeight - element.scrollTop === element.clientHeight)\
\      {\
\          return true;\
\      }\
\      else {\
\        return false;\
\     }\
\   }\
\   }\
\  ())"
  checkScroll :: JSString -> IO Bool

-- Components
-- | a div element that calls the IO () function in the case of a scrollbox hitting the bottom.
--   Used to implement infinite scroll
scrollBoxWith :: IO () -> JSString -> [Property] -> S.Seq LiveVDom -> LiveVDom
scrollBoxWith scrollFunction elementId props children =
 flip addProps props $ addEvent (scrollCheck elementId scrollFunction) tb
  where
    tb = LiveVNode [] "div" Nothing [Property "id" (JSPString elementId)] children

-- | A basic button component with the default of accepting an STM Address
button :: Address (Event ()) -> [Property] -> JSString -> LiveVDom
button addr = buttonWith $ sendMessage addr $ Fired ()

-- | Add a button where you can send/receive non-blocking
-- messages with the Message monad
-- when the button is pressed
buttonWith :: Message b -> [Property] -> JSString -> LiveVDom
buttonWith f props text = (flip addProps) props $ addEvent (EV.click (const $ void $ runMessages f)) $
  LiveVNode [] "button" Nothing [Property "type" $ JSPString "button"] $ S.fromList [StaticText [] text]

spanWith :: Message b -> [Property] -> LiveVDom
spanWith f props = (flip addProps) props $ addEvent (EV.click (const $ void $ runMessages f)) $
  LiveVNode [] "span" Nothing [] $ S.empty

buttonWithKids :: Message b -> [Property] -> JSString -> S.Seq LiveVDom -> LiveVDom
buttonWithKids f props text children = (flip addProps) props $ addEvent (EV.click (const $ void $ runMessages f)) $
  LiveVNode [] "button" Nothing [Property "type" $ JSPString "button"] $ (S.><) (S.fromList [StaticText  [] text]) children

label :: Address (Event JSString) -> [Property] -> JSString -> LiveVDom
label addr = labelWith $ \str -> sendMessage addr $ Fired str

-- | A label with a click event
labelWith :: (JSString -> Message b) -> [Property] -> JSString -> LiveVDom
labelWith f props str = (flip addProps) props $ addEvent (clickToGetDivText $ \str -> void . runMessages $ f str) l
  where
    l = LiveVNode [] "div" Nothing [] $ S.fromList [StaticText [] str]


-- | A textbox with type="text" that updates the given address with the
-- current value of the textbox each time the textbox is updated
textBox :: Address (Event JSString) -> [Property] -> Maybe JSString -> LiveVDom
textBox addr = textBoxWith $ \str -> sendMessage addr $ Fired str

-- | A textbox that allows you to send non-blocking messages with the Message
-- monad whenever the input changes
textBoxWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
textBoxWith f props mStr = flip addProps props $ addInput tb
  where
    tb = LiveVNode [] "input" Nothing
                   ([Property "type" $ JSPString "text"])
                   S.empty
    addInput = addEvent (input $ \str -> void . runMessages $ f str)
    
inputWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
inputWith f props mStr = (flip addProps) props $ addInput $ addKeyPress $ addKeyUp tb
  where
    tb = LiveVNode [] "input" Nothing [] S.empty
    addInput = addEvent (input $ \str -> void . runMessages $ f str)
    addKeyPress = addEvent (keypress $ \str -> void . runMessages $ f str)
    addKeyUp = addEvent (keyup $ \str -> void . runMessages $ f str)

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBox :: Address (Event Int) -> LiveVDom
numberBox addr = addInput $ addEvent (inputChange f ) tb
  where tb = LiveVNode [] "input" Nothing [Property "type" $ JSPString "text"] S.empty
        addInput = addEvent (input $ f)
        f = \str -> void . Data.Traversable.sequence $ sendIO addr <$> Fired <$> (readMaybe str)
-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBoxWith :: (Event Int -> Message b) -> LiveVDom
numberBoxWith f = addInput $ addEvent (inputChange $ runWithFunction ) tb
  where tb = LiveVNode [] "input" Nothing [Property "type" $ JSPString "number"] S.empty
        maybeToEvent (Nothing) = Unfired
        maybeToEvent (Just e) = Fired e
        runWithFunction = \str -> void . runMessages . f . maybeToEvent $ readMaybe str
        addInput = addEvent (input runWithFunction)


doubleBoxWith :: (Double -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
doubleBoxWith f props mStr = (flip addProps) props $ addInput $  addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    addInput = addEvent (input $ \str -> void . runMessages $ f str)
    tb = LiveVNode [] "input" Nothing
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [Property "type" $ JSPString "number"])
                   S.empty


-- | A dropdown list
-- General implementation of selectList1
selectList :: (Eq v)
           => Map.Map JSString v -- ^ Map from strings (displayed in the menu) to values
           -> (v -> Message ()) -- ^ Selection handler, given the value associated with the selected string
           -> [Property] -- ^ Extra properties for the select element
           -> Maybe v -- ^ Default selection
           -> LiveVDom
selectList kvMap = selectList1 kvMap defaultDisplayFunc defaultDisplayFunc
  where defaultDisplayFunc (k,_) = k


-- | A dropdown list with custom function to derive selector name from key-value pairs
selectList1 :: (FromJSVal k, Show k, Ord k, Eq v)
            => Map.Map k v -- ^ Map from strings (displayed in the menu) to values
            -> ((k, v) -> JSString) -- ^ Function to generate selector name in option
            -> ((k, v) -> JSString) -- ^ Function to generate selector value in option
            -> (v -> Message ()) -- ^ Selection handler, given the value associated with the selected string
            -> [Property] -- ^ Extra properties for the select element
            -> Maybe v -- ^ Default selection
            -> LiveVDom
selectList1 kvMap optionFunc valFunc messageFunc props mSelected =
    (flip addProps) props
  $ addEvents [ (inputChange $ \str -> runMessages $ lookupKey str), (keydown $ \str -> runMessages $ lookupKey str)]
  $ LiveVNode [] "select" Nothing []
  $ options
  where lookupKey s = case Map.lookup s kvMap of
                        (Nothing) -> debug $ "Error looking up " ++ (show s)
                        (Just val) -> messageFunc val
        pleaseSelect = do
          let noDefault = case mSelected of
                              Nothing -> True
                              Just _ -> False
          LiveVNode [] "option" Nothing [Property "selected" $ JSPBool noDefault, Property "disabled" $ JSPBool True] $ S.fromList [StaticText [] "Please Select"]
        items = fmap (\t@(_,v) -> option' (Just v == mSelected) (valFunc t) (optionFunc t)) (S.fromList $ Map.toList kvMap)
        options = pleaseSelect S.<| items



-- | A dropdown list built from non-string keys and a function to derive selector names from key-value pairs
selectListWith :: (Ord k, Eq v) => ((k,v) -> JSString) -> Map.Map k v -> (v -> Message ()) -> [Property] -> Maybe v -> LiveVDom
selectListWith buildDisplay kvMap = selectList displayMap
  where displayMap = Map.fromList $ (\t@(_,v) -> (buildDisplay t, v) ) <$> Map.toList kvMap



option' :: Bool
        -> JSString    -- value
        -> JSString    -- option
        -> LiveVDom
option' selected val opt = LiveVNode [] "option" Nothing ((if selected then ((Property "selected" $ JSPBool True):) else id) $ [Property "value" $ JSPString val]) $ S.fromList [StaticText [] opt]


option :: Bool -> JSString -> LiveVDom
option selected opt = option' selected opt opt
















-- | This is a workhorse of live-vdom, it is used to build messages across mailboxes
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

-- | A little wrapper around the applicative instance
-- on STMEnvelope but allows for updating the current value
-- as well
withMailbox :: STMMailbox a
              -> (a -> (a -> Message ()) -> LiveVDom)
              -> STMEnvelope (LiveVDom)
withMailbox mb@(env, _) buildFunc = buildDom <$> env
  where buildDom value = buildFunc value (\newValue -> modifyMailbox mb (const newValue))





















--------------------------------------------------
-- Components that Michael added, need to decide which ones are general enough to be used in many projects
-- which ones should be built locally
--------------------------------------------------

inputSubmit :: Address (Event ()) -> [Property] -> JSString -> LiveVDom
inputSubmit addr = inputSubmitWith (sendMessage addr $ Fired ())

inputSubmitWith :: Message b -> [Property] -> JSString -> LiveVDom
inputSubmitWith f props text = (flip addProps) props $ addEvent (EV.click (const $ void $ runMessages f)) $
  LiveVNode [] "input" Nothing [Property "type" $ JSPString "submit"] $ S.fromList [StaticText [] text]

-- | like textBoxWith, but takes addition function that gets called when enter key is pressed
textBoxWithEnterKey :: (JSString -> Message b) -> Message () -> [Property] -> Maybe JSString -> LiveVDom
textBoxWithEnterKey f enterFunc props mStr = (flip addProps) props $ addKeyPress $ addKeyUp tb
  where
    tb = LiveVNode [] "input" Nothing
                   ([Property "type" $ JSPString "text"])
                   S.empty
    addKeyPress = addEvent (keypressCheckEnter (\str -> void . runMessages $ f str) (void $ runMessages enterFunc))
    addKeyUp = addEvent (keyup $ \str -> void . runMessages $ f str)

passwordBoxWith :: (JSString -> Message b) -> Message () -> [Property] -> Maybe JSString -> LiveVDom
passwordBoxWith f enterFunc props mStr = (flip addProps) props $ addKeyPress $ addKeyUp tb
  where
    tb = LiveVNode [] "input" Nothing
                   ([Property "type" $ JSPString "password"])
                   S.empty
    addKeyPress = addEvent (keypressCheckEnter (\str -> void . runMessages $ f str) (void $ runMessages enterFunc))
    addKeyUp = addEvent (keyup $ \str -> void . runMessages $ f str)

textAreaWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
textAreaWith  f props mStr = (flip addProps) props $ addKeyPress $ addKeyUp tb
  where
    tb = LiveVNode [] "textarea" Nothing
                   ([Property "type" $ JSPString "password"])
                   S.empty
    addKeyPress = addEvent (keypress $ \str -> void . runMessages $ f str)
    addKeyUp = addEvent (keyup $ \str -> void . runMessages $ f str)

formWith :: (JSString -> Message b) -> [Property] -> S.Seq LiveVDom -> Maybe JSString -> LiveVDom
formWith f props children mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "form" Nothing
                   ([])
                   children

linkWith :: (JSString -> Message b) -> [Property] -> S.Seq LiveVDom -> Maybe JSString -> LiveVDom
linkWith f props children mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "a" Nothing
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [])
                   children

imageWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> LiveVDom
imageWith f props mStr = (flip addProps) props $ addEvent (keypress $ \str -> void . runMessages $ f str) tb
  where
    tb = LiveVNode [] "img" Nothing
                   (maybe id ((:) . Property "value" . JSPString) mStr
                     [])
                   S.empty

