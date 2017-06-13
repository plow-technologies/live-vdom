{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ConstraintKinds            #-}

module LiveVDom.Internal (
   Elem
  , Desc
  , Identity(..)
  , ToDesc(..)
  , child'
  , children'
  , liveChildren'
  , liveChild'
  , text'
  , liveText'
  , elem'
  , elemNS'
  , spawn'
  , addProps'
  , addAttribute'
  , renderElement
  , renderDescription
  , buildStaticText
  , buildLiveVNode
  , buildLiveChild
  , buildLiveChildren
  , buildLiveVText
  , liveText
  , staticText'
  , liveVNode
  , liveChild
  , liveChildren
  , liftElem
) where

import           Control.Concurrent.STM.Notify

import           Data.Monoid
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as S

import           Data.JSString                 (JSString)

import           GHCJS.VDOM.Attribute
import           LiveVDom.Adapter.Types
import qualified LiveVDom.Types                as T
import           LiveVDom.UserTypes

import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Free      hiding (Free, Pure)
import           Data.Functor.Identity
import           GHC.Exts (Constraint)


buildLiveVText :: Functor m => STMEnvelope JSString -> Desc m ()
buildLiveVText =
  liveText'

buildStaticText :: Functor m => JSString -> Desc m ()
buildStaticText =
  text'

buildLiveVNode :: Functor m =>
                   TagName
                -> Maybe JSString
                -> [Property]
                -> Seq (Desc m ())
                -> Desc m ()
buildLiveVNode tagName (Just namespace) props ch =
  child' $ elemNS' tagName [] props namespace $ sequence_ ch
buildLiveVNode tagName Nothing props ch =
  child' $ elem' tagName [] props $ sequence_ ch

buildLiveChild :: Functor m => STMEnvelope (Elem Identity) -> Desc m ()
buildLiveChild = liveChild'

buildLiveChildren :: Desc m () -> Desc m ()
buildLiveChildren x = x

-- | Handle things that can be hosted in Desc
class ToDesc a where
  -- | The effect of a keeps track of what can and can't
  -- be described inside of Desc
  type Effect a (m :: * -> *) :: Constraint
  type Effect a m = ()

  toDesc :: Effect a f => a -> Desc f ()


instance ToDesc (Desc Identity ()) where
  type Effect (Desc Identity ()) f = Monad f
  toDesc d = liftDesc d

instance ToDesc (Desc IO ()) where
  type Effect (Desc IO ()) f = (f ~ IO)
  toDesc d = d

instance ToDesc (Elem Identity) where
  type Effect (Elem Identity) f = Monad f
  toDesc = staticChild

instance ToDesc (Elem IO) where
  type Effect (Elem IO) f = (f ~ IO)
  toDesc = child'

instance ToDesc a => ToDesc (Seq a) where
  type Effect (Seq a) f = (Functor f, Effect a f)
  toDesc = mapM_ toDesc

instance ToDesc (STMEnvelope JSString) where
  type Effect (STMEnvelope JSString) m = Functor m
  toDesc = liveText'

instance ToDesc JSString where
  type Effect JSString m = Functor m
  toDesc = text'

instance ToDesc (STMEnvelope (Desc Identity ())) where
  type Effect (STMEnvelope (Desc Identity ())) m = Functor m
  toDesc = liveDesc'

instance ToDesc (STMEnvelope (Seq (Desc Identity ()))) where
  type Effect (STMEnvelope (Seq (Desc Identity ()))) m = Functor m
  toDesc = liveDesc' . fmap sequence_

instance ToDesc (STMEnvelope (Elem Identity)) where
  type Effect (STMEnvelope (Elem Identity)) m = Functor m
  toDesc = liveChild'

instance ToDesc (STMEnvelope (Seq (Elem Identity))) where
  type Effect (STMEnvelope (Seq (Elem Identity))) m = Functor m
  toDesc = liveChildren'

-- | An Element is a single HTML Element
-- with Attributes and Properties being seperated,
-- a description of children elements,
-- and possibly a namespace
data Elem m = Elem {
  _elemName         :: TagName
, _elemAttributes   :: [Attribute]
, _elemProperties   :: [Property]
, _elemNamespace    :: Maybe JSString
, _elemDescriptions :: Desc m ()
}

-- | Internal Free definition of the Desc monad
data FDesc m next =
    Child (Elem m) next
  | LiveChild (STMEnvelope (Elem Identity)) next
  | LiveChildren (STMEnvelope (Seq (Elem Identity))) next
  | LiveDesc (STMEnvelope (Desc Identity ())) next
  | VText JSString next
  | LiveVText (STMEnvelope JSString) next
  | RunOnce (m next)

instance Functor f => Functor (FDesc f) where
  fmap f (Child c n)         = Child c (f n)
  fmap f (LiveChildren cs n) = LiveChildren cs (f n)
  fmap f (LiveChild c n)     = LiveChild c (f n)
  fmap f (LiveDesc c n)      = LiveDesc c (f n)
  fmap f (VText t n)         = VText t (f n)
  fmap f (LiveVText lt n)    = LiveVText lt (f n)
  fmap f (RunOnce act)       = RunOnce $ f <$> act

{-| A description of the construction of
    children elements. The two builtin uses of m are IO and Identity.

    In IO, runOnce actions can be performed where they are ran once and bound.

    Identity means no effects can take place.

    However, because of the semantics of spawning elements, you can't embed
    STMEnvelope (Desc IO ()) in any piece.
-}
data Desc m a = Desc {
  _unDesc :: Free (FDesc m) a
}

instance Functor f => Functor (Desc f) where
  fmap f (Desc x) = Desc $ fmap f x

instance Functor f => Applicative (Desc f) where
  pure x = Desc $ pure x
  (Desc f) <*> (Desc x) = Desc $ f <*> x

instance Functor f => Monad (Desc f) where
  (Desc x) >>= f = Desc $ x >>= (_unDesc . f)


instance (Monoid a, Functor f) => Monoid (Desc f a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b


-- | Create a child element description
child' :: Functor m => Elem m -> Desc m ()
child' e =
  Desc $ liftF $ Child e ()

staticChild :: Monad f => Elem Identity -> Desc f ()
staticChild =
  liftDesc . child'

-- | Create a group of children elements
children' :: Functor m => Seq (Elem m) -> Desc m ()
children' es =
  sequence_ $ child' <$> es

-- | Have a list of children inside a mutable container
liveChildren' :: Functor m => STMEnvelope (Seq (Elem Identity)) -> Desc m ()
liveChildren' es =
  Desc $ liftF $ LiveChildren es ()

-- | Have a single child inside of a mutable container
liveChild' :: Functor m => STMEnvelope (Elem Identity) -> Desc m ()
liveChild' e =
  Desc $ liftF $ LiveChild e ()

-- | A static text node
text' :: Functor m => JSString -> Desc m ()
text' text =
  Desc $ liftF $ VText text ()

-- | Mutable text node
liveText' :: Functor m => STMEnvelope JSString -> Desc m ()
liveText' text =
  Desc $ liftF $ LiveVText text ()

-- | Mutable description of children
liveDesc' :: Functor m => STMEnvelope (Desc Identity ()) -> Desc m ()
liveDesc' ch =
  Desc $ liftF $ LiveDesc ch ()

-- | Smart constructor for an element with no namespace
elem' :: TagName
      -> [Attribute]
      -> [Property]
      -> Desc m ()
      -> Elem m
elem' tagName attributes properties ch =
  Elem tagName attributes properties Nothing ch

-- | Smart constructor for an element with a namespace
elemNS' :: TagName
        -> [Attribute]
        -> [Property]
        -> JSString
        -> Desc m ()
        -> Elem m
elemNS' tagName attributes properties ns ch =
  Elem tagName attributes properties (Just ns) ch

-- | Add a list of properties to an element
addProps' :: [Property] -> Elem m -> Elem m
addProps' props (Elem tagName attributes properties namespace ch) =
  Elem tagName attributes (properties ++ props) namespace ch

-- | Add a list of attributes to an element
addAttribute' :: [Attribute] -> Elem m -> Elem m
addAttribute' attrs' (Elem tagName attrs props ns ch) =
  Elem tagName (attrs ++ attrs') props ns ch

-- | Spawn a mailbox inside of Desc
spawn' :: a -> Desc IO (STMMailbox a)
spawn' v =
  Desc $ liftF $ RunOnce $ spawnIO v

-- | Convert to LiveVDom given the rendered children
elemToVDom :: Elem m -> Seq LiveVDom -> LiveVDom
elemToVDom (Elem tagName atr prop namespace _) ch =
  T.LiveVNode atr tagName namespace prop ch

-- | Replace the children of an element with
-- a new children description
updateChildren :: Elem f -> Desc m () -> Elem m
updateChildren (Elem tagName atr prop namespace _) d' =
  Elem tagName atr prop namespace d'

-- | Render an element to LiveVDom and update all runOnce occurences to
-- be Identity. If m was IO, the effects will performed
renderElement :: (Monad m) => Elem m -> m (LiveVDom, Elem Identity)
renderElement e@(Elem tagName atr prop namespace desc) = do
  (children , desc') <- renderDescription desc
  return
    (elemToVDom e children
    , updateChildren e desc')

-- | Render a description resulting in a Seq LiveVDom and performing all
-- side effects in runOnce.
-- All runOnce are performed and moved into Identity
renderDescription :: (Monad m) => Desc m a -> m (Seq LiveVDom, Desc Identity a)
renderDescription (Desc d) =
  fmap Desc <$> go d
  where
    go :: (Monad m) => Free (FDesc m) a -> m (Seq LiveVDom, Free (FDesc Identity) a)
    go (Pure x) =
      return (S.empty, Pure x)
    go (Free (Child c n)) = do
      (child, e') <- renderElement c
      (nextChildren, n') <- go n
      return (child S.<| nextChildren, Free (Child e' n'))
    go (Free (LiveChildren cs n)) = do
      (cs', n') <- go n
      let
        t = (fmap . fmap) (fst . runIdentity . renderElement) cs
      return (T.LiveChildren [] t S.<| cs', Free (LiveChildren cs n'))
    go (Free (LiveChild c n)) = do
      (cs', n') <- go n
      let c' = fst <$> runIdentity . renderElement <$> c
      return (T.LiveChild [] c' S.<| cs', Free (LiveChild c n'))
    go (Free (LiveDesc ch n)) = do
      (cs', n') <- go n
      let ch' = fst <$> runIdentity . renderDescription <$> ch
      return (T.LiveChildren [] ch' S.<| cs', Free (LiveDesc ch n'))
    go (Free (VText t n)) = do
      (cs', n') <- go n
      return (T.StaticText [] t S.<| cs', Free (VText t n'))
    go (Free (LiveVText lt n)) = do
      (cs', n') <- go n
      return (T.LiveVText [] lt S.<| cs', Free (LiveVText lt n'))
    go (Free (RunOnce act)) = do
      res <- act
      (ch, res') <- go res
      return $ (ch, Free (RunOnce $ Identity res'))

-- | Smart constructor for mutable text
liveText :: [Attribute] -> STMEnvelope JSString -> LiveVDom
liveText = T.LiveVText

-- | Smart constructor for static text
staticText' :: [Attribute] -> JSString -> LiveVDom
staticText' = T.StaticText

-- | Smart constructor for a static node
liveVNode :: TagName -> [Attribute] -> [Property] -> Seq LiveVDom -> LiveVDom
liveVNode tagName attrs props ch =
  T.LiveVNode attrs tagName Nothing props ch

-- | Smart constructor for a mutable node
liveChild :: [Attribute] -> STMEnvelope LiveVDom -> LiveVDom
liveChild = T.LiveChild

-- | Smart constructor for a mutable list of children
liveChildren :: [Attribute] -> STMEnvelope (Seq LiveVDom) -> LiveVDom
liveChildren = T.LiveChildren

-- | Lift an element in Identity to an element another monad
liftElem :: Monad m => Elem Identity -> Elem m
liftElem (Elem name attr prop ns desc) =
  Elem name attr prop ns $ liftDesc desc


liftDesc :: Monad m => Desc Identity a -> Desc m a
liftDesc (Desc d) =
  Desc $ go d
  where
    go (Pure x) = Pure x
    go (Free (Child c n)) = Free $ Child (liftElem c) $ go n
    go (Free (LiveChildren cs n)) = Free $ LiveChildren cs $ go n
    go (Free (LiveDesc c n)) = Free $ LiveDesc c $ go n
    go (Free (LiveChild c n)) = Free $ LiveChild c $ go n
    go (Free (VText t n)) = Free $ VText t $ go n
    go (Free (LiveVText lt n)) = Free $ LiveVText lt $ go n
    go (Free (RunOnce act)) = Free $ RunOnce $ return . go . runIdentity $ act
