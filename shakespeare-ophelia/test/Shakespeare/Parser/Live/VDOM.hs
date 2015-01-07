{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Shakespeare.Parser.Live.VDOM where


import           Prelude

import           Text.Parser.Combinators
import           Text.Trifecta.Combinators
import qualified Text.Trifecta.Delta             as D
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Test.Hspec

import           Data.String.Here
import qualified Data.Text                       as T
import qualified Data.Tree                       as T
import           Shakespeare.Ophelia.Parser
import           Shakespeare.Parser

import           Shakespeare.Ophelia.Parser.VDOM
import           Shakespeare.Ophelia.QQ
import           VDOM.Adapter

import           Pipes.Concurrent



specLiveNode = do
  specSimpleLiveNode


specSimpleLiveNode = do
  describe "simple live node" $ do
    it "compose the child nodes correctly" $ do
      x <- atomically . recv . toProducer $ testNode1 (return testNode2)
      y <- atomically . recv $ toProducer testNode1Res 
      x `shouldBe` y



testNode2 = [gertrude|
<some other="test">
  <with a="child">
|]


testNode1 inp = [gertrude|
<some header="value">
  <with children="nodes">
    !{id inp}
  <and some="other values">
|]


testNode1Res = [gertrude|
<some header="value">
  <with children="nodes">
    <some other="test">
      <with a="child">
  <and some="other values">
|]