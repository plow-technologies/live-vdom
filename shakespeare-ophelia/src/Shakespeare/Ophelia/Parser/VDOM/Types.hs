{-# LANGUAGE NoImplicitPrelude #-}
module Shakespeare.Ophelia.Parser.VDOM.Types where

import           Control.Applicative
import           Control.Monad              hiding (sequence, mapM)
import           Data.Traversable
-- import           Pipes.Concurrent -- Not used because of stm-notify
import           Control.Concurrent.STM.Notify
import           Prelude                    hiding (sequence, mapM)

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           VDOM.Adapter

data LiveVDom =
     LiveVText {liveVirtualText :: String } -- ^ Child text with  no tag name, properties, or children
   | LiveVNode {liveVNodeTagName :: TagName, liveVNodePropsList :: [Property], liveVNodeChildren :: [LiveVDom]} -- ^ Basic tree structor for a node with children and properties
   | LiveChild {liveVChild :: STMEnvelope LiveVDom} -- ^ Mutable dom
   | LiveChildren {liveVChildren :: STMEnvelope [LiveVDom]}



data PLiveVDom =
     PLiveVText {pLiveVirtualText :: String } -- ^ Child text with  no tag name, properties, or children
   | PLiveVNode {pLiveVNodeTagName :: TagName, pLiveVNodePropsList :: [Property], pLiveVNodeChildren :: [PLiveVDom]} -- ^ Basic tree structor for a node with children and properties
   | PLiveChild {pLiveVChild :: Exp}         -- ^ A parsed TH Exp that will get turned into LiveChild
   | PLiveChildren {pLiveVChildren :: Exp}         -- ^ A parsed TH Exp that will get turned into LiveChildren
   | PLiveInterpText  {pLiveInterpText :: Exp} -- ^ Interpolated text that will get transformed into LiveVText

instance Lift PLiveVDom where
  lift (PLiveVText st) = AppE (ConE $ mkName "PLiveVText") <$> (lift st)
  lift (PLiveVNode tn pl ch) = do
    qtn <- lift tn
    qpl <- lift pl
    qch <- lift ch
    return $ AppE (AppE (AppE (ConE $ mkName "PLiveVNode") qtn) qpl) qch
  lift (PLiveChild e) = return e
  lift (PLiveChildren e) = return e
  lift (PLiveInterpText t) = return t

-- | Use template haskell to create the live vdom
toLiveVDomTH :: PLiveVDom -> Q Exp
toLiveVDomTH (PLiveVText st) = AppE (ConE $ mkName "LiveVText") <$> (lift st)
toLiveVDomTH (PLiveVNode tn pl ch) = do
  qtn <- lift tn
  qpl <- lift pl
  cExp <- sequence $ toLiveVDomTH <$> ch
  return $ AppE (AppE (AppE (ConE $ mkName "LiveVNode") qtn) qpl) (ListE cExp)
toLiveVDomTH (PLiveChild e) = return $ AppE (ConE  $ mkName "LiveChild") e
toLiveVDomTH (PLiveChildren e) = return $ AppE (ConE  $ mkName "LiveChildren") e
toLiveVDomTH (PLiveInterpText t) = return $ AppE (ConE $ mkName "LiveVText") t


-- | Transform LiveDom to VNode so that it can be processed
toProducer :: LiveVDom -> STMEnvelope [VNodeAdapter]
toProducer (LiveVText t) = return $ [VText t]
toProducer (LiveVNode tn pl ch) = do
  ch' <- mapM toProducer ch
  return $ [VNode tn pl (join ch')]
toProducer (LiveChild ivc) = join $ toProducer <$> ivc
toProducer (LiveChildren lvc) = do
  xs <- join $ sequence <$> (fmap toProducer) <$> lvc
  return $ join xs
