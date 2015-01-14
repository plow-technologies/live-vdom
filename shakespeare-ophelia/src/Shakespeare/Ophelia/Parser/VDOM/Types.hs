{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
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
   | LiveChild {liveVChild :: STMEnvelope LiveVDom} -- ^ DOM that can change
   | LiveChildren {liveVChildren :: STMEnvelope [LiveVDom]} -- ^ A child that can change



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
toLiveVDomTH (PLiveVText st) = AppE (ConE 'LiveVText) <$> (lift st)
toLiveVDomTH (PLiveVNode tn pl ch) = do
  qtn <- lift tn
  qpl <- lift pl
  cExp <- sequence $ toLiveVDomTH <$> ch
  return $ AppE (AppE (AppE (ConE 'LiveVNode) qtn) qpl) (ListE cExp)
toLiveVDomTH (PLiveChild e) = return $ AppE (ConE  'LiveChild) e
toLiveVDomTH (PLiveChildren e) = return $ AppE (ConE  'LiveChildren) e
toLiveVDomTH (PLiveInterpText t) = return $ AppE (ConE 'LiveVText) t


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



-- | Add a list of property to LiveVNode if it is a liveVNode
  -- If it isn't it leaves the rest alone
addProps :: LiveVDom -> [Property] -> LiveVDom
addProps (LiveVNode tn pl ch) pl' = LiveVNode tn (pl ++ pl') ch
addProps l _ = l