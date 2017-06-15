{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , forEachIn
  , forEachInWithIndex
  , spanWith
  , withMailbox
  , scrollBoxWith
  , option
  ) where

import           Control.Concurrent.STM.Notify


import           LiveVDom.Event

import           LiveVDom.Adapter
import           LiveVDom.Adapter.Types
import           LiveVDom.UserTypes

import           Control.Monad
import           Data.Bool                     (bool)
import qualified Data.Map                      as Map
import           Data.Maybe                    (isNothing, maybeToList)
import qualified Data.Sequence                 as S
import qualified Data.Traversable              as T
import           GHCJS.VDOM.Attribute          hiding (id)
import qualified GHCJS.VDOM.Event              as EV
import           LiveVDom.Message
import           Text.Read

import           GHCJS.Foreign.QQ
import           GHCJS.Types

import           GHCJS.Marshal
import           Unsafe.Coerce

--ghcjs-base

import           LiveVDom.Internal



{- BUG - buildEvent
  looks like there is delay from when the event gets called
  and when the variable gets passed to the event if you add
  a threadDelay it causes some lag but the variable is passed.
  If there is no delay then the variable will not make it until the next event
-}

-- Events

-- | Get the innerHTML of a div on click
clickToGetDivText :: (FromJSVal b) => (b -> IO ()) -> Attribute
clickToGetDivText f = EV.click $ \ev -> do
  mVal <- getCurrentInnerHTML $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing  -> return ()

-- | Get the current input value on the 'input' event
inputChange :: (FromJSVal b) => (b -> IO ()) -> Attribute
inputChange f = EV.change $ \ev -> do
  mVal <- getCurrentValue $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing  -> putStrLn "input fail"


-- | Get the current value of an element on keypress
keypress :: (FromJSVal b) => (b -> IO ()) -> Attribute
keypress f = EV.keypress $ \ev -> do
  mVal <- getCurrentValue $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing  -> putStrLn "Keypress fail"

-- | Similar to keypress, but takes an addition function
--   param which gets called instead if the enter key is pressed
keypressCheckEnter :: (FromJSVal b) => (b -> IO ()) -> IO () -> Attribute
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

-- | Get the current value of an element on keydown
keydown :: (FromJSVal b) => (b -> IO ()) -> Attribute
keydown f = EV.keydown $ \ev -> do
  mVal <- getCurrentValue $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing  -> putStrLn "keydown fail"

-- | Get the current value of an element on keyup
keyup :: (FromJSVal b) => (b -> IO ()) -> Attribute
keyup f = EV.keyup $ \ev -> do
  mVal <- getCurrentValue $ unsafeCoerce ev
  case mVal of
    (Just v) -> f v
    Nothing  -> putStrLn "keyup fail"

-- | Check for reaching the bottom of the element
scrollCheck :: JSString -> IO () -> Attribute
scrollCheck elementId infiniteScrollFunction = EV.scroll $ \_ -> do
  isBottom <- checkScroll elementId
  when isBottom infiniteScrollFunction

-- This was changed from the original version
-- but it is assumed that if the element is null
-- the result should be false.
--checkScroll function
foreign import javascript unsafe "(function () {\
\  var element = document.getElementById($1);\
\  if (element) {\
\    if (element.scrollHeight - element.scrollTop === element.clientHeight) {\
\      return true;\
\    }\
\    else {\
\      return false;\
\    }\
\  }\
\  return false;\
\  }\
\  ())"
  checkScroll :: JSString -> IO Bool

-- Components
-- | a div element that calls the IO () function in the case of
-- a scrollbox hitting the bottom. Used to implement infinite scroll
scrollBoxWith :: IO () -> JSString -> [Property] -> Desc m () -> Elem m
scrollBoxWith scrollFunction elementId props children =
  elem' "div" [scrollEvent] properties children
  where
    properties = idProp:props
    idProp = Property "id" $ JSPString elementId
    scrollEvent = scrollCheck elementId scrollFunction

-- | A basic button component with the default of accepting an STM Address
button :: Address (Event ()) -> [Property] -> JSString -> Elem Identity
button addr =
  buttonWith $ sendMessage addr $ Fired ()

-- | Add a button where you can send/receive non-blocking
-- messages with the Message monad
-- when the button is pressed
buttonWith :: Message b -> [Property] -> JSString -> Elem Identity
buttonWith f props buttonLabel =
  elem' "button" [onClick] properties $ text' buttonLabel
  where
    properties = typeButton:props
    typeButton = Property "type" $ JSPString "button"
    onClick = EV.click $ const . void $ runMessages f

-- | A span with an onclick event
spanWith :: Message b -> [Property] -> Elem Identity
spanWith f props =
  elem' "span" [onClick] props mempty
  where
    onClick = EV.click $ const . void $ runMessages f

-- | Button with an onclick event
buttonWithKids :: Message b -> [Property] -> JSString -> Desc m () -> Elem m
buttonWithKids f props _text children = -- TODO fix children
  elem' "button" [onClick] properties $ children
  where
    properties = typeButton:props
    typeButton = Property "type" $ JSPString "button"
    onClick = EV.click $ const . void $ runMessages f

-- | a label with an on click event that sends the text
-- to the given Address
label :: Address (Event JSString) -> [Property] -> JSString -> Elem Identity
label addr =
  labelWith $ sendMessage addr . Fired

-- | A label with a click event
-- that sends the text
labelWith :: (JSString -> Message b) -> [Property] -> JSString -> Elem Identity
labelWith f props str =
  elem' "div" [divTextEvent] props $ text' str
  where
    divTextEvent = clickToGetDivText $ void . runMessages . f

-- | A textbox with type="text" that updates the given address with the
-- current value of the textbox each time the textbox is updated
textBox :: Address (Event JSString) -> [Property] -> Maybe JSString -> Elem Identity
textBox addr =
  textBoxWith $ sendMessage addr . Fired

-- | A textbox that allows you to send non-blocking messages with the Message
-- monad whenever the input changes
textBoxWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> Elem Identity
textBoxWith f props _mStr =
  elem' "input" [keyUpEvent, keyPressEvent] properties mempty
  where
    properties = typeText:props
    typeText = Property "type" $ JSPString "text"
    keyPressEvent = keypress $ void . runMessages . f
    keyUpEvent = keyup $ void . runMessages . f

inputWith :: (JSString -> Message b) -> [Property] -> Maybe JSString -> Elem Identity
inputWith f props _mStr =
  elem' "input" [keyUpEvent, keyPressEvent] props mempty
  where
    keyPressEvent = keypress $ void . runMessages . f
    keyUpEvent = keyup $ void . runMessages . f

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBox :: Address (Event Int) -> Elem Identity
numberBox addr =
  elem' "input" [inputEvent] [Property "type" $ JSPString "text"] mempty
  where inputEvent = inputChange $ sendOutput . readInput
        readInput inp = Fired <$> readMaybe inp
        sendOutput out = void . T.sequence $ sendIO addr <$> out

-- | The same as a textbox with string but it parses the string to a number and
-- has type="numberBox"
numberBoxWith :: (Event Int -> Message b) -> Elem Identity
numberBoxWith f =
  elem' "input" [inputEvent] [Property "type" $ JSPString "number"] mempty
  where inputEvent =
          inputChange $ void . runMessages . f . maybeToEvent . readMaybe
        maybeToEvent (Nothing) = Unfired
        maybeToEvent (Just e)  = Fired e

doubleBoxWith :: (Double -> Message b) -> [Property] -> Maybe JSString -> Elem Identity
doubleBoxWith f props mStr =
  elem' "input" [inputEvent] properties mempty
  where
    properties = typeProp:(valueProp ++ props)
    valueProp = maybeToList $ Property "value" . JSPString <$> mStr
    typeProp = Property "type" $ JSPString "number"
    inputEvent = keypress $ void . runMessages . f



-- | A dropdown list
-- General implementation of selectList1
selectList :: (Eq v)
           => Map.Map JSString v
           -- ^ Map from strings (displayed in the menu) to values
           -> (v -> Message ())
           -- ^ Selection handler, given the value
           -- associated with the selected string
           -> [Property]
           -- ^ Extra properties for the select element
           -> Maybe v
           -- ^ Default selection
           -> Elem Identity
selectList kvMap = selectList1 kvMap defaultDisplayFunc defaultDisplayFunc
  where defaultDisplayFunc (k,_) = k


-- | A dropdown list with custom function to
--   derive selector name from key-value pairs
selectList1 :: (FromJSVal k, Show k, Ord k, Eq v)
            => Map.Map k v
            -- ^ Map from strings (displayed in the menu) to values
            -> ((k, v) -> JSString)
            -- ^ Function to generate selector name in option
            -> ((k, v) -> JSString)
            -- ^ Function to generate selector value in option
            -> (v -> Message ())
            -- ^ Selection handler, given the value associated
            --  with the selected string
            -> [Property]
            -- ^ Extra properties for the select element
            -> Maybe v
            -- ^ Default selection
            -> Elem Identity
selectList1 kvMap optionFunc valFunc messageFunc props mSelected =
  elem' "select" [onInputEvent, onKeyDownEvent] props $ toDesc options
  where
    lookupKey s =
      case Map.lookup s kvMap of
        (Nothing)  -> debug $ "Error looking up " ++ show s
        (Just val) -> messageFunc val
    pleaseSelect :: Elem Identity
    pleaseSelect =
      elem'
        "option"
        []
        [Property "selected" $ JSPBool (isNothing mSelected)
        , Property "disabled" $ JSPBool True]
        $ text' "Please Select"
    items = S.fromList $ fmap createOption $ Map.toList kvMap
    createOption t@(_,v) =
      option' (Just v == mSelected) (valFunc t) (optionFunc t)
    options = pleaseSelect S.<| items
    onInputEvent = inputChange $ runMessages . lookupKey
    onKeyDownEvent = keydown $ runMessages . lookupKey



-- | A dropdown list built from non-string
-- keys and a function to derive selector names from key-value pairs
selectListWith :: (Ord k, Eq v)
               => ((k,v) -> JSString)
               -> Map.Map k v
               -> (v -> Message ())
               -> [Property]
               -> Maybe v
               -> Elem Identity
selectListWith buildDisplay kvMap =
  selectList displayMap
  where
    displayMap = Map.fromList $ runBuildDisplay <$> Map.toList kvMap
    runBuildDisplay t@(_,v) = (buildDisplay t, v)



option' :: Bool
        -> JSString    -- value
        -> JSString    -- option
        -> Elem Identity
option' selected val opt =
  elem' "option" [] props $ text' opt
  where
    selectedProp = bool [] [Property "selected" $ JSPBool True] selected
    props = valProp:selectedProp
    valProp = Property "value" $ JSPString val


option :: Bool -> JSString -> Elem Identity
option selected opt = option' selected opt opt
















-- | This is a workhorse of live-vdom,
-- it is used to build messages across mailboxes
forEach :: (ToDesc (STMEnvelope (S.Seq d)), Effect (STMEnvelope d) Identity, Effect (STMEnvelope (S.Seq d)) Identity)
        => STMMailbox (S.Seq a) -- ^ Values to map over
        -> (a -> (Maybe a -> Message ()) -> d)
        -- ^ Function to generate dom given an element
        -- and a function to change the current value
        -> Desc Identity () -- STMEnvelope (S.Seq (Desc Identity ())) -- Desc Identity ()
forEach mb func = toDesc $ (fmap buildDom) <$> withIndices
  where withIndices = S.zip <$> stmIndexList <*> env
        stmIndexList = (increasingSeq . S.length) <$> env
        increasingSeq = S.fromList . ((flip take) [0,1..])
        buildDom (i, val) = func val (updateValue i)
        updateValue i (Just newVal) = modifyMailbox mb (S.update i newVal)
        updateValue i _             = modifyMailbox mb (remove i)
        env = fst mb
        remove i ts = appendL  $ S.viewl <$> S.splitAt i ts
        appendL (xs,(_ S.:< ys)) = xs S.>< ys
        appendL (xs,_)           = xs


-- | Apply "loop" to a specific record field
--
-- The difference between this and 'forEach' is that 'forEach' focuses the "loop"
-- on a mailbox sequence. There are times that your sequence/list is wrapped within a type.
-- Then, this is useful for that case.
--
-- Example:
--
-- data Classroom = Classroom { name :: String, students :: Seq Student }
--
-- With 'forEach', to iterate `students`, you would have to have something like `STMMailbox (Seq Student)`.
-- However, this is not possible because you can't  transform 'STMMailbox Classroom to 'STMMailbox (Seq Student)
-- by doing 'students <$> (example :: STMMailbox Classroom)' because 'Address' is not a functor.
-- Remember, STMMailbox Classroom = (STMEnvelope Classroom, Address Classroom)
forEachIn :: forall a b. STMMailbox a                   -- ^ Mailbox
          -> (a -> S.Seq b)                                -- ^ function to extract the 'Foldable' part of 'a' into 'Seq b'
          -> (a -> S.Seq b -> a)                           -- ^ function to "merge" 'Seq b' into its original 'a'
          -> (b -> (Maybe b -> Message ()) -> Desc Identity ())    -- ^ function to generate dom given an element and a function to change the current value
          -> STMEnvelope (S.Seq (Desc Identity ()))
forEachIn mb@(stmEnv, _) convSeq convObj func = (fmap buildDom) <$> itemsWithIdx
  where
    itemsWithIdx :: STMEnvelope (S.Seq (Int, b))
    itemsWithIdx = withIndices <$> convSeq <$> stmEnv

    withIndices :: S.Seq b -> S.Seq (Int, b)
    withIndices xs = S.zip (seqIndexList xs) xs

    seqIndexList :: S.Seq b -> S.Seq Int
    seqIndexList = increasingSeq . S.length

    increasingSeq :: Int -> S.Seq Int
    increasingSeq = S.fromList . ((flip take) [0,1..])

    updateSeq :: Int -> b -> S.Seq b -> S.Seq b
    updateSeq idx newVal = S.update idx newVal

    buildDom :: (Int, b) -> Desc Identity ()
    buildDom (idx, val) = func val (updateValue idx)

    updateValue :: Int -> Maybe b -> Message ()
    updateValue idx (Just newVal) = modifyMailbox mb (\a -> convObj a $ updateSeq idx newVal (convSeq a))
    updateValue idx _             = modifyMailbox mb (\a -> convObj a $ remove idx (convSeq a))

    remove :: Int -> S.Seq b -> S.Seq b
    remove i ts = appendL  $ S.viewl <$> S.splitAt i ts

    appendL :: (S.Seq b, S.ViewL b) -> S.Seq b
    appendL (xs,(_ S.:< ys)) = xs S.>< ys
    appendL (xs,_          ) = xs


-- | Same as `forEachIn` but with index
forEachInWithIndex
  :: forall a b. STMMailbox a
  -> (a -> S.Seq b)
  -> (a -> S.Seq b -> a)
  -> (Int -> b -> (Maybe b -> Message ()) -> Desc Identity ())
  -> STMEnvelope (S.Seq (Desc Identity ()))
forEachInWithIndex mb@(stmEnv, _) convSeq convObj func = (fmap buildDom) <$> itemsWithIdx
  where
    itemsWithIdx :: STMEnvelope (S.Seq (Int, b))
    itemsWithIdx = withIndices <$> convSeq <$> stmEnv

    withIndices :: S.Seq b -> S.Seq (Int, b)
    withIndices xs = S.zip (seqIndexList xs) xs

    seqIndexList :: S.Seq b -> S.Seq Int
    seqIndexList = increasingSeq . S.length

    increasingSeq :: Int -> S.Seq Int
    increasingSeq = S.fromList . ((flip take) [0,1..])

    updateSeq :: Int -> b -> S.Seq b -> S.Seq b
    updateSeq idx newVal = S.update idx newVal

    buildDom :: (Int, b) -> Desc Identity ()
    buildDom (idx, val) = func idx val (updateValue idx)

    updateValue :: Int -> Maybe b -> Message ()
    updateValue idx (Just newVal) = modifyMailbox mb (\a -> convObj a $ updateSeq idx newVal (convSeq a))
    updateValue idx _             = modifyMailbox mb (\a -> convObj a $ remove idx (convSeq a))

    remove :: Int -> S.Seq b -> S.Seq b
    remove i ts = appendL  $ S.viewl <$> S.splitAt i ts

    appendL :: (S.Seq b, S.ViewL b) -> S.Seq b
    appendL (xs,(_ S.:< ys)) = xs S.>< ys
    appendL (xs,_          ) = xs


-- | A little wrapper around the applicative instance
-- on STMEnvelope but allows for updating the current value
-- as well
withMailbox :: (ToDesc (STMEnvelope d), Effect (STMEnvelope d) Identity)
            => STMMailbox a
            -> (a -> (a -> Message ()) -> d)
            -> Desc Identity ()
withMailbox mb@(env, _) buildFunc =
  toDesc $ buildDom <$> env
  where
    buildDom v =
      buildFunc v (\newValue -> modifyMailbox mb (const newValue))

--------------------------------------------------
-- Components that Michael added, need to decide
-- which ones are general enough to be used in many projects
-- and which ones should be built locally
--------------------------------------------------

inputSubmit :: Address (Event ()) -> [Property] -> JSString -> Elem Identity
inputSubmit addr = inputSubmitWith (sendMessage addr $ Fired ())

inputSubmitWith :: Message b -> [Property] -> JSString -> Elem Identity
inputSubmitWith f props textLabel =
  elem' "input" [onClickEvent] properties
    $ text' textLabel
  where
    properties = typeSubmit:props
    typeSubmit = Property "type" $ JSPString "submit"
    onClickEvent = EV.click $ const . void $ runMessages f

-- | like textBoxWith, but takes addition function that
-- gets called when enter key is pressed
textBoxWithEnterKey :: (JSString -> Message b)
                    -> Message ()
                    -> [Property]
                    -> Maybe JSString
                    -> Elem Identity
textBoxWithEnterKey f enterFunc props _mStr =
  elem' "input" [onKeyPressEvent, onKeyUpEvent] properties mempty
  where
    properties = typeText:props
    typeText = Property "type" $ JSPString "text"
    onKeyPressEvent =
      keypressCheckEnter
        (void . runMessages . f)
        (void $ runMessages enterFunc)
    onKeyUpEvent = keyup $ void . runMessages . f

passwordBoxWith :: (JSString -> Message b)
                -> Message ()
                -> [Property]
                -> Maybe JSString
                -> Elem Identity
passwordBoxWith f enterFunc props _mStr =
  elem' "input" [onKeyPressEvent, onKeyUpEvent] properties mempty
  where
    properties = typePassword:props
    typePassword = Property "type" $ JSPString "password"
    onKeyPressEvent =
      keypressCheckEnter
        (void . runMessages . f)
        (void $ runMessages enterFunc)
    onKeyUpEvent = keyup $ void . runMessages . f

textAreaWith :: (JSString -> Message b)
             -> [Property]
             -> Maybe JSString
             -> Elem Identity
textAreaWith  f props _mStr =
  elem' "textarea" [onKeyPressEvent, onKeyUpEvent] props mempty
  where
    onKeyPressEvent = keypress $ void . runMessages . f
    onKeyUpEvent = keyup $ void . runMessages . f

formWith :: (JSString -> Message b)
         -> [Property]
         -> Desc m ()
         -> Elem m
formWith f props children =
  elem' "form" [onKeyPressEvent] props children
  where
    onKeyPressEvent = keypress $ void . runMessages . f

linkWith :: (JSString -> Message b)
         -> [Property]
         -> Desc m ()
         -> Maybe JSString
         -> Elem m
linkWith f props children mStr =
  elem' "a" [onKeyPressEvent] properties children
  where
    properties = valueProp ++ props
    valueProp = maybeToList $ Property "value" . JSPString <$> mStr
    onKeyPressEvent = keypress $ void . runMessages . f


imageWith :: (JSString -> Message b)
          -> [Property]
          -> Maybe JSString
          -> Elem Identity
imageWith f props mStr =
  elem' "img" [onKeyPressEvent] properties mempty
  where
    properties = valueProp ++ props
    valueProp = maybeToList $ Property "value" . JSPString <$> mStr
    onKeyPressEvent = keypress $ void . runMessages . f
