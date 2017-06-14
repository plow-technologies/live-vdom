{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module LiveVDom.Types where

-- Generic imports
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Notify
import           Control.Monad                 hiding (mapM, mapM_, sequence)
import           Data.Foldable                 (mapM_, toList, traverse_)
import           Data.Monoid
import           Data.Sequence                 ((|>))
import qualified Data.Sequence                 as S
import           Data.Traversable
import           Prelude                       hiding (mapM, mapM_, sequence)

-- Template haskell related
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- VDOM related
import           Data.String
import           GHCJS.VDOM.Attribute
import           LiveVDom.Adapter.Types

--ghcjs-base
import           Data.JSString                 (JSString)
import qualified Data.JSString                 as JS (pack, unpack)

newtype DomLoc = DomLoc { unDomLoc :: Int } deriving (Eq, Show)
type ElementLoc = [DomLoc]


domAt :: ElementLoc -> LiveVDom -> LiveVDom
domAt ((DomLoc i):xs) (LiveVNode _ _ _ _ children) = domAt xs $ S.index children i


instance (IsString a) => IsString (STMEnvelope a) where
  fromString = return . fromString

-- | Resulting type from the quasiquoted valentine
data LiveVDom =
     LiveVText {liveVTextEvents :: ![Attribute], liveVirtualText :: STMEnvelope JSString } -- ^ Child text with  no tag name, properties, or children
   | StaticText { staticTextEvents :: ![Attribute], staticText :: {-# UNPACK #-} !JSString }
   | LiveVNode { liveVNodeEvents :: ![Attribute]
               , liveVNodeTagName :: {-# UNPACK #-} !TagName
               , liveVNodeNameSpace :: {-# UNPACK  #-} !(Maybe JSString)
               , liveVNodePropsList :: {-# UNPACK #-} ![Property]
               , liveVNodeChildren :: !(S.Seq (LiveVDom))} -- ^ Basic tree structor for a node with children and properties
   | LiveChild { liveVChildEvents :: ![Attribute], liveVChild :: STMEnvelope (LiveVDom)} -- ^ DOM that can change
   | LiveChildren {liveVChildEvents :: ![Attribute], liveVChildren :: STMEnvelope (S.Seq (LiveVDom))} -- ^ A child that can change

-- |The instance on Monoid is designed to make it easy to paste together nodes in a for each kind of way
-- notably blending children and adding childs
-- However the text conditions are terminal
-- Events are kept with the incoming LiveVDom
-- append is like:
{-|
>>> let foo = [valentine| <div>
                             <div>
                                  <div>
                                  <div> |]

let bar = [valentine| <div> |]

$> foo <> bar
<div>
   <div>
      <div>
      <div>
         <bar>

let baz = [valentine| <div>
                         some text which will erase everything
          |]

let bing = [valentine| <div> |]

$> baz <> bing
<div>
   some text which will erase everything

|-}

instance Monoid LiveVDom where
  mempty = StaticText []  ""
  mappend nodeL nodeR = case nodeL of
           (StaticText [] "")  -> nodeR -- memtpy law RHS
           txt@(LiveVText _ _ )  -> txt
           txt@(StaticText _ _)  -> txt -- Notice this means that all text is terminal (with respect to the monoid)!!!
           (LiveVNode as tag nameSpace props children) -> LiveVNode as tag nameSpace props (appendToLastChild nodeR children )
           (LiveChild es env)  ->  LiveChildren es $ fmap (appendToLastChild nodeR) (S.singleton <$> env)
           lc@(LiveChildren es env)  -> LiveChildren es (fmap (appendToLastChild nodeR) env)
    where
      appendToLastChild node children
         |S.null children = S.singleton node
         |otherwise = let index = S.length children  - 1
                      in S.adjust (<> node) index children

-- | A template haskell representation for parsing
data PLiveVDom =
     PLiveVText {pLiveVirtualText :: JSString } -- ^ Child text with  no tag name, properties, or children
   | PLiveVNode {pLiveVNodeTagName :: TagName
                , pLiveVNodeNameSpace :: Maybe JSString
                , pLiveVNodePropsList :: [Property]
                , pLiveVNodeChildren :: [PLiveVDom]} -- ^ Basic tree structor for a node with children and properties
   | PLiveChildren {pLiveVChild :: Exp}         -- ^ A parsed TH Exp that will get turned into LiveChild
   | PLiveInterpText  {pLiveInterpText :: Exp} -- ^ Interpolated text that will get transformed into LiveVText
  deriving (Show,Eq)

instance Lift PLiveVDom where
  lift (PLiveVText st) = AppE (ConE 'PLiveVText) <$> (lift $ JS.unpack st)
  lift (PLiveVNode tn ns pl ch) = do
    qtn <- lift tn
    qns <- lift $ fmap JS.unpack ns
    qpl <- lift pl
    qch <- lift ch
    return $ AppE (AppE (AppE (AppE (ConE 'PLiveVNode) qtn) qns) qpl) qch
  lift (PLiveChildren e) = return e
  lift (PLiveInterpText t) = return t

-- | Add an event to a LiveVDom
addEvent :: Attribute -> LiveVDom -> LiveVDom
addEvent ev (LiveVText evs ch) = LiveVText (evs ++ [ev]) ch -- Child text with  no tag name, properties, or children
addEvent ev (LiveVNode evs tn ns pls ch) = LiveVNode (evs ++ [ev]) tn ns pls ch -- Basic tree structor for a node with children and properties
addEvent ev (LiveChild evs vch) = LiveChild (evs ++ [ev]) vch -- DOM that can change
addEvent ev (LiveChildren evs vchs) = LiveChildren (evs ++ [ev]) vchs -- A child that can change

-- | Add multiple events to LiveVDom
addEvents :: [Attribute] -> LiveVDom -> LiveVDom
addEvents ev (LiveVText evs ch) = LiveVText (evs ++ ev) ch -- Child text with  no tag name, properties, or children
addEvents ev (StaticText evs ch) = StaticText (evs ++ ev) ch -- Child text with  no tag name, properties, or children
addEvents ev (LiveVNode evs tn ns pls ch) = LiveVNode (evs ++ ev) tn ns pls ch -- Basic tree structor for a node with children and properties
addEvents ev (LiveChild evs vch) = LiveChild (evs ++ ev) vch -- DOM that can change
addEvents ev (LiveChildren evs vchs) = LiveChildren (evs ++ ev) vchs

-- | Add a list of property to LiveVNode if it is a liveVNode
-- If it isn't it leaves the rest alone
addProps :: LiveVDom -> [Property] -> LiveVDom
addProps (LiveVNode evs tn ns pl ch) pl' = LiveVNode evs tn ns (pl ++ pl') ch
addProps l _ = l

-- | Append a list of children to LiveVDom
addChildren :: S.Seq (LiveVDom) -> LiveVDom -> LiveVDom
addChildren children (LiveVText _ _) = error "Error: Text node can't have children"
addChildren children (StaticText _ _) = error "Error: Static text nodes can't have children"
addChildren children (LiveVNode evs tn ns pls ch) = LiveVNode evs tn ns pls $ ch S.>< children
addChildren children (LiveChild _ _) = error "Error: LiveChild node can't have children"
addChildren children (LiveChildren evs vchs) = LiveChildren evs $ (S.>< children) <$> vchs

-- | Append a single child to LiveVDom
addChild :: LiveVDom -> LiveVDom -> LiveVDom
addChild x lv = addChildren (S.singleton x) lv

-- | add a dom listener to a a given node and all children of that node
addDomListener :: TMVar () -> LiveVDom -> STM ()
addDomListener tm (LiveVText _ t) = addListener t tm
addDomListener tm (StaticText _ t) = return ()
addDomListener tm (LiveVNode _ _ _ _ ch) = traverse_ (addDomListener tm) ch
addDomListener tm (LiveChild _ vch) = (addListener vch tm) >>
                                            (addDomListener tm =<< recv vch)
addDomListener tm (LiveChildren _ vchs) = do
  addListener vchs tm
  xs <- recv vchs
  mapM_ (addDomListener tm) xs


-- | Wait for a change in LiveVDom
-- this recursively adds an empty tmvar to each element
-- and waits for a change
waitForDom :: STMEnvelope (LiveVDom) -> IO ()
waitForDom envDom = do
  dom <- recvIO envDom
  listener <- atomically $ do
    listen <- newEmptyTMVar
    addListener envDom listen
    addDomListener listen dom
    return listen
  atomically $ readTMVar listener
