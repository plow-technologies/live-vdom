{-# LANGUAGE NoImplicitPrelude #-}
module Shakespeare.Ophelia.Parser.Live.Types where

import           Control.Applicative
import           Control.Monad              hiding (sequence)
import           Data.Traversable
import           Pipes.Concurrent
import           Prelude                    hiding (sequence)

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           VDOM.Adapter

data LiveVDom =
     LiveVText {liveVirtualText :: String } -- ^ Child text with  no tag name, properties, or children
   | LiveVNode {liveVNodeTagName :: TagName, liveVNodePropsList :: [Property], liveVNodeChildren :: [LiveVDom]} -- ^ Basic tree structor for a node with children and properties
   | LiveChild {liveVChild :: Input LiveVDom}



data PLiveVDom =
     PLiveVText {pLiveVirtualText :: String } -- ^ Child text with  no tag name, properties, or children
   | PLiveVNode {pLiveVNodeTagName :: TagName, pLiveVNodePropsList :: [Property], pLiveVNodeChildren :: [PLiveVDom]} -- ^ Basic tree structor for a node with children and properties
   | PLiveChild {pLiveVChild :: Exp}
   | PLiveText  {pLiveText :: Exp}

instance Lift PLiveVDom where
  lift (PLiveVText st) = AppE (ConE $ mkName "PLiveVText") <$> (lift st)
  lift (PLiveVNode tn pl ch) = do
    qtn <- lift tn
    qpl <- lift pl
    qch <- lift ch
    return $ AppE (AppE (AppE (ConE $ mkName "PLiveVNode") qtn) qpl) qch
  lift (PLiveChild e) = return e
  lift (PLiveText t) = return t

-- | Use template haskell to create the live vdom
toLiveVDomTH :: PLiveVDom -> Q Exp
toLiveVDomTH (PLiveVText st) = AppE (ConE $ mkName "LiveVText") <$> (lift st)
toLiveVDomTH (PLiveVNode tn pl ch) = do
  qtn <- lift tn
  qpl <- lift pl
  cExp <- sequence $ toLiveVDomTH <$> ch
  return $ AppE (AppE (AppE (ConE $ mkName "LiveVNode") qtn) qpl) (ListE cExp)
toLiveVDomTH (PLiveChild e) = return $ AppE (ConE  $ mkName "LiveChild") e
toLiveVDomTH (PLiveText t) = return $ AppE (ConE $ mkName "LiveVText") t


-- | Transform LiveDom to VNode so that it can be processed
toProducer :: LiveVDom -> Input VNodeAdapter
toProducer (LiveVText t) = return $ VText t
toProducer (LiveVNode tn pl ch) = (VNode tn pl) <$> (Data.Traversable.sequence $ toProducer <$> ch)
toProducer (LiveChild ivc) = join $ toProducer <$> ivc
