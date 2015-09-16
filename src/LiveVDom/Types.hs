{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module LiveVDom.Types where


-- Generic imports
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Notify
import           Control.Monad                 hiding (mapM, mapM_, sequence)
import           Data.Foldable                 (mapM_, toList, traverse_)
import qualified Data.Sequence                 as S
import           Data.Traversable
import           Prelude                       hiding (mapM, mapM_, sequence)


-- Template haskell related
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- VDOM related
import           Data.String
import           LiveVDom.Adapter.Types


instance (IsString a) => IsString (STMEnvelope a) where
  fromString = return . fromString

-- | Resulting type from the quasiquoted valentine
data LiveVDom a =
     LiveVText {liveVTextEvents :: [a], liveVirtualText :: STMEnvelope String } -- ^ Child text with  no tag name, properties, or children
   | LiveVNode {liveVNodeEvents :: [a], liveVNodeTagName :: TagName, liveVNodePropsList :: [Property], liveVNodeChildren :: (S.Seq (LiveVDom a))} -- ^ Basic tree structor for a node with children and properties
   | LiveChild {liveVChildEvents :: [a], liveVChild :: STMEnvelope (LiveVDom a)} -- ^ DOM that can change
   | LiveChildren {liveVChildEvents :: [a], liveVChildren :: STMEnvelope (S.Seq (LiveVDom a))} -- ^ A child that can change


-- | Type that valentine is parsed into
data PLiveVDom =
     PLiveVText {pLiveVirtualText :: String } -- ^ Child text with  no tag name, properties, or children
   | PLiveVNode {pLiveVNodeTagName :: TagName, pLiveVNodePropsList :: [Property], pLiveVNodeChildren :: [PLiveVDom]} -- ^ Basic tree structor for a node with children and properties
   | PLiveChild {pLiveVChild :: Exp}         -- ^ A parsed TH Exp that will get turned into LiveChild
   | PLiveChildren {pLiveVChildren :: Exp}         -- ^ A parsed TH Exp that will get turned into LiveChildren
   | PLiveInterpText  {pLiveInterpText :: Exp} -- ^ Interpolated text that will get transformed into LiveVText

instance Lift PLiveVDom where
  lift (PLiveVText st) = AppE (ConE 'PLiveVText) <$> (lift st)
  lift (PLiveVNode tn pl ch) = do
    qtn <- lift tn
    qpl <- lift pl
    qch <- lift ch
    return $ AppE (AppE (AppE (ConE 'PLiveVNode) qtn) qpl) qch
  lift (PLiveChild e) = return e
  lift (PLiveChildren e) = return e
  lift (PLiveInterpText t) = return t

-- | Use template haskell to create the live vdom
toLiveVDomTH :: PLiveVDom -> Q Exp
toLiveVDomTH (PLiveVText st) = do
  iStr <- lift st
  return $ AppE (AppE (ConE 'LiveVText) (ListE [])) iStr
toLiveVDomTH (PLiveVNode tn pl ch) = do
  qtn <- lift tn
  qpl <- lift pl
  cExp <- sequence $ toLiveVDomTH <$> ch
  return $ AppE (AppE (AppE (AppE (ConE 'LiveVNode) (ListE [])) qtn) qpl) (AppE (VarE 'S.fromList) (ListE cExp))
toLiveVDomTH (PLiveChild e) = return $ AppE (AppE (ConE  'LiveChild) (ListE [])) e
toLiveVDomTH (PLiveChildren e) = return $ AppE (AppE (ConE  'LiveChildren) (ListE [])) e
toLiveVDomTH (PLiveInterpText t) = return $ AppE (AppE (ConE 'LiveVText) (ListE [])) t


-- | Transform LiveDom to VNode so that it can be processed
toProducer :: LiveVDom JSEvent -> STMEnvelope (S.Seq VNodeAdapter)
toProducer (LiveVText ev t) = (\text -> S.singleton $ VText ev text) <$> t
toProducer (LiveVNode ev tn pl ch) = do
  ch' <- traverse toProducer ch
  return . S.singleton $ VNode ev tn pl $ toList (join ch')
toProducer (LiveChild ev ivc) = join $ toProducer <$> (addEvents ev <$> ivc)
toProducer (LiveChildren ev lvc) = do
  xs <- join $ sequence <$> (fmap (toProducer . addEvents ev)) <$> lvc
  return $ join xs

-- | Add an event to a LiveVDom
addEvent :: a -> LiveVDom a -> LiveVDom a
addEvent ev (LiveVText evs ch) = LiveVText (evs ++ [ev]) ch -- Child text with  no tag name, properties, or children
addEvent ev (LiveVNode evs tn pls ch) = LiveVNode (evs ++ [ev]) tn pls ch -- Basic tree structor for a node with children and properties
addEvent ev (LiveChild evs vch) = LiveChild (evs ++ [ev]) vch -- DOM that can change
addEvent ev (LiveChildren evs vchs) = LiveChildren (evs ++ [ev]) vchs -- A child that can change

-- | Add multiple events to LiveVDom
addEvents :: [a] -> LiveVDom a -> LiveVDom a
addEvents ev (LiveVText evs ch) = LiveVText (evs ++ ev) ch -- Child text with  no tag name, properties, or children
addEvents ev (LiveVNode evs tn pls ch) = LiveVNode (evs ++ ev) tn pls ch -- Basic tree structor for a node with children and properties
addEvents ev (LiveChild evs vch) = LiveChild (evs ++ ev) vch -- DOM that can change
addEvents ev (LiveChildren evs vchs) = LiveChildren (evs ++ ev) vchs

-- | Add a list of property to LiveVNode if it is a liveVNode
-- If it isn't it leaves the rest alone
addProps :: LiveVDom a -> [Property] -> LiveVDom a
addProps (LiveVNode evs tn pl ch) pl' = LiveVNode evs tn (pl ++ pl') ch
addProps l _ = l


-- | add a dom listener to a a given node and all children of that node
addDomListener :: TMVar () -> LiveVDom a -> IO ()
addDomListener tm (LiveVText _ t) = atomically $ addListener t tm
addDomListener tm (LiveVNode _ _ _ ch) = traverse_ (addDomListener tm) ch
addDomListener tm (LiveChild _ vch) = (atomically $ addListener vch tm) >>
                                            (addDomListener tm =<< recvIO vch)
addDomListener tm (LiveChildren _ vchs) = do
  atomically $ addListener vchs tm
  xs <- recvIO vchs
  mapM_ (addDomListener tm) xs

waitForDom :: STMEnvelope (LiveVDom a) -> IO ()
waitForDom envDom = do
  dom <- recvIO envDom
  listener <- newEmptyTMVarIO
  atomically $ addListener envDom $ listener
  addDomListener listener dom
  atomically $ readTMVar listener
