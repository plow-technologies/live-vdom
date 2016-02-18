{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      : LiveVDom.Combinators
Description : Alignment and composition of LiveVdom
Copyright   : Plow Technologies LLC
License     : MIT License



LiveVDom has some properties that are very similar to something like diagrams.

These combinators try and copy some of that functionality.
it is not included in the regular import LiveVDom module set for now.




| -}
module LiveVDom.Combinators where
-- Generic imports
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Notify
import           Control.Monad                 hiding (mapM, mapM_, sequence)
import           Data.Foldable                 (mapM_, toList, traverse_)
import           Data.Monoid
import           Data.Sequence                 ((<|), (|>))
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

--local
import           LiveVDom.Types


{-|
Combine two LivdVDom elements so they are siblings
This will purposefully demote LivdVNode to a Children status
Which means if you make a stack of verticle elements you must feed it to a parent.

>>> let foo = [valentine|
<div>
  Helper
|]

>>> let bar = [valentine|
<div>
  Hurter
|]

>>> let parent = [valentine| <parent> |]

>>> parent <> (foo
               ===
               bar)
<parent>
  <div>
    Helper
  <div>
    Hurter


|-}

(<<>>) :: (LiveVDom a) -> LiveVDom a -> LiveVDom a
(<<>>) vdomT vdomB = case vdomT of
                          (StaticText [] "")  -> vdomB -- memtpy law RHS
                          txt@(LiveVText _ _ )  -> txt
                          txt@(StaticText _ _)  -> txt -- Notice this means that all text is terminal (with respect to the monoid)!!!
                          (LiveVNode as tags props children) -> LiveChildren [] (spawnChildren (vdomT <| vdomB <| S.empty))
                          (LiveChild es env)  ->  LiveChildren [] (spawnChildren (vdomT <| vdomB <| S.empty))
                          lc@(LiveChildren es env)  -> LiveChildren [] (spawnChildren (vdomT <| vdomB <| S.empty))
      where
        spawnChildren c = do
            (val,_) <- spawnEnvelope c
            val
