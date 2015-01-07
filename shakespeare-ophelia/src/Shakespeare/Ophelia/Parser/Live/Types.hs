module Shakespeare.Ophelia.Parser.Live.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Traversable
import           Pipes.Concurrent

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

instance Lift PLiveVDom where
  lift (PLiveVText st) = AppE (ConE $ mkName "PLiveVText") <$> (lift st)
  lift (PLiveVNode tn pl ch) = AppE <$> (AppE <$> (AppE (ConE $ mkName "PLiveVNode") <$> (lift tn)) <*> (lift pl)) <*> (lift ch)
  lift (PLiveChild e) = return e

-- | Use template haskell to create the live vdom
toLiveVDomTH :: PLiveVDom -> Q Exp
toLiveVDomTH (PLiveVText st) = AppE (ConE $ mkName "LiveVText") <$> (lift st)
toLiveVDomTH (PLiveVNode tn pl ch) = AppE <$> (AppE <$> (AppE (ConE $ mkName "LiveVNode") <$> (lift tn)) <*> (lift pl)) <*> (lift ch)
toLiveVDomTH (PLiveChild e) = return $ AppE (ConE  $ mkName "LiveChild") e


-- | Transform LiveDom to VNode so that it can be processed
toProducer :: LiveVDom -> Input VNodeAdapter
toProducer (LiveVText t) = return $ VText t
toProducer (LiveVNode tn pl ch) = (VNode tn pl) <$> (Data.Traversable.sequence $ toProducer <$> ch)
toProducer (LiveChild ivc) = join $ toProducer <$> ivc
