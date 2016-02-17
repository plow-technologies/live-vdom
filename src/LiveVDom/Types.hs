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


domAt :: ElementLoc -> LiveVDom a -> LiveVDom a
domAt ((DomLoc i):xs) (LiveVNode _ _ _ children) = domAt xs $ S.index children i


instance (IsString a) => IsString (STMEnvelope a) where
  fromString = return . fromString

-- | Resulting type from the quasiquoted valentine
data LiveVDom a =
     LiveVText {liveVTextEvents :: [a], liveVirtualText :: STMEnvelope JSString } -- ^ Child text with  no tag name, properties, or children
   | StaticText { staticTextEvents :: [a], staticText :: JSString }
   | LiveVNode {liveVNodeEvents :: [a], liveVNodeTagName :: TagName, liveVNodePropsList :: [Property], liveVNodeChildren :: (S.Seq (LiveVDom a))} -- ^ Basic tree structor for a node with children and properties
   | LiveChild {liveVChildEvents :: [a], liveVChild :: STMEnvelope (LiveVDom a)} -- ^ DOM that can change
   | LiveChildren {liveVChildEvents :: [a], liveVChildren :: STMEnvelope (S.Seq (LiveVDom a))} -- ^ A child that can change


-- The instance on Monoid is designed to make it easy to paste together nodes in a for each kind of way
-- notably blending children and adding childs
-- However the text conditions are terminal
-- Events are kept with the incoming LiveVDom
-- Except in the case of a Child being appended to another child or children
instance Monoid (LiveVDom a) where
  mempty = StaticText []  ""
  mappend (StaticText [] "") node = node -- memtpy law LHS
  mappend node (StaticText [] "")  = node -- memtpy law RHS
  mappend txt@(LiveVText _ _ ) _ = txt
  mappend txt@(StaticText _ _) node = txt
  mappend (LiveVNode as tags props children) node = LiveVNode as tags props (children |> node)
  mappend (LiveChild es env) node =  (LiveChildren es ((S.empty |> ) <$> env)) `mappend` node
  mappend lc@(LiveChildren es env) node = evaluateRHS node
     where
       evaluateRHS (LiveChild es' env') = LiveChildren (es <> es' ) ( (|>) <$> env <*> env')
       evaluateRHS (LiveChildren es' env') = LiveChildren (es <> es') ((<>)  <$> env <*> env')
       evaluateRHS l = LiveChildren es ((|> l) <$> env)

-- | A template haskell representation for parsing
data PLiveVDom =
     PLiveVText {pLiveVirtualText :: JSString } -- ^ Child text with  no tag name, properties, or children
   | PLiveVNode {pLiveVNodeTagName :: TagName, pLiveVNodePropsList :: [Property], pLiveVNodeChildren :: [PLiveVDom]} -- ^ Basic tree structor for a node with children and properties
   | PLiveChild {pLiveVChild :: Exp}         -- ^ A parsed TH Exp that will get turned into LiveChild
   | PLiveChildren {pLiveVChildren :: Exp}         -- ^ A parsed TH Exp that will get turned into LiveChildren
   | PLiveInterpText  {pLiveInterpText :: Exp} -- ^ Interpolated text that will get transformed into LiveVText
   | PStaticVNode { pStaticVNode :: Exp }      -- ^ A static node of LiveVDom
   | PStaticText { pStaticText :: Exp }        -- ^ A static node of just text

instance Lift PLiveVDom where
  lift (PLiveVText st) = AppE (ConE 'PLiveVText) <$> (lift $ JS.unpack st)
  lift (PLiveVNode tn pl ch) = do
    qtn <- lift tn
    qpl <- lift pl
    qch <- lift ch
    return $ AppE (AppE (AppE (ConE 'PLiveVNode) qtn) qpl) qch
  lift (PLiveChild e) = return e
  lift (PLiveChildren e) = return e
  lift (PLiveInterpText t) = return t
  lift (PStaticVNode n) = return n
  lift (PStaticText t) = return t

-- | Use template haskell to create the live vdom
-- the resulting type is LiveVDom
toLiveVDomTH :: PLiveVDom -> Q Exp
toLiveVDomTH (PLiveVText st) = do
  iStr <- lift $ JS.unpack st
  return $ AppE (AppE (ConE 'StaticText) (ListE [])) iStr
toLiveVDomTH (PLiveVNode tn pl ch) = do
  qtn <- lift tn
  qpl <- lift pl
  cExp <- sequence $ toLiveVDomTH <$> ch
  return $ AppE (AppE (AppE (AppE (ConE 'LiveVNode) (ListE [])) qtn) qpl) (AppE (VarE 'S.fromList) (ListE cExp))
toLiveVDomTH (PLiveChild e) = return $ AppE (AppE (ConE  'LiveChild) (ListE [])) e
toLiveVDomTH (PLiveChildren e) = return $ AppE (AppE (ConE  'LiveChildren) (ListE [])) e
toLiveVDomTH (PLiveInterpText t) = return $ AppE (AppE (ConE 'LiveVText) (ListE [])) t
toLiveVDomTH (PStaticVNode e) = return e
toLiveVDomTH (PStaticText t) = return $ AppE (AppE (ConE 'StaticText) (ListE [])) t

-- | Add an event to a LiveVDom
addEvent :: a -> LiveVDom a -> LiveVDom a
addEvent ev (LiveVText evs ch) = LiveVText (evs ++ [ev]) ch -- Child text with  no tag name, properties, or children
addEvent ev (LiveVNode evs tn pls ch) = LiveVNode (evs ++ [ev]) tn pls ch -- Basic tree structor for a node with children and properties
addEvent ev (LiveChild evs vch) = LiveChild (evs ++ [ev]) vch -- DOM that can change
addEvent ev (LiveChildren evs vchs) = LiveChildren (evs ++ [ev]) vchs -- A child that can change

-- | Add multiple events to LiveVDom
addEvents :: [a] -> LiveVDom a -> LiveVDom a
addEvents ev (LiveVText evs ch) = LiveVText (evs ++ ev) ch -- Child text with  no tag name, properties, or children
addEvents ev (StaticText evs ch) = StaticText (evs ++ ev) ch -- Child text with  no tag name, properties, or children
addEvents ev (LiveVNode evs tn pls ch) = LiveVNode (evs ++ ev) tn pls ch -- Basic tree structor for a node with children and properties
addEvents ev (LiveChild evs vch) = LiveChild (evs ++ ev) vch -- DOM that can change
addEvents ev (LiveChildren evs vchs) = error "LiveVDom.Types: addEvents, This shouldn't be used. Please report this as a bug" -- LiveChildren (evs ++ ev) vchs

-- | Add a list of property to LiveVNode if it is a liveVNode
-- If it isn't it leaves the rest alone
addProps :: LiveVDom a -> [Property] -> LiveVDom a
addProps (LiveVNode evs tn pl ch) pl' = LiveVNode evs tn (pl ++ pl') ch
addProps l _ = l

-- | Append a list of children to LiveVDom
addChildren :: S.Seq (LiveVDom a) -> LiveVDom a -> LiveVDom a
addChildren children (LiveVText _ _) = error "Error: Text node can't have children"
addChildren children (StaticText _ _) = error "Error: Static text nodes can't have children"
addChildren children (LiveVNode evs tn pls ch) = LiveVNode evs tn pls $ ch S.>< children
addChildren children (LiveChild _ _) = error "Error: LiveChild node can't have children"
addChildren children (LiveChildren evs vchs) = LiveChildren evs $ (S.>< children) <$> vchs

-- | Append a single child to LiveVDom
addChild :: LiveVDom a -> LiveVDom a -> LiveVDom a
addChild x lv = addChildren (S.singleton x) lv

-- | add a dom listener to a a given node and all children of that node
addDomListener :: TMVar () -> LiveVDom a -> STM ()
addDomListener tm (LiveVText _ t) = addListener t tm
addDomListener tm (StaticText _ t) = return ()
addDomListener tm (LiveVNode _ _ _ ch) = traverse_ (addDomListener tm) ch
addDomListener tm (LiveChild _ vch) = (addListener vch tm) >>
                                            (addDomListener tm =<< recv vch)
addDomListener tm (LiveChildren _ vchs) = do
  addListener vchs tm
  xs <- recv vchs
  mapM_ (addDomListener tm) xs


-- | Wait for a change in LiveVDom
-- this recursively adds an empty tmvar to each element
-- and waits for a change
waitForDom :: STMEnvelope (LiveVDom a) -> IO ()
waitForDom envDom = do
  dom <- recvIO envDom
  listener <- atomically $ do
    listen <- newEmptyTMVar
    addListener envDom listen
    addDomListener listen dom
    return listen
  atomically $ readTMVar listener
