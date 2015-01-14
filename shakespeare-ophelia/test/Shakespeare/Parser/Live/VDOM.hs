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

import           Control.Concurrent.STM.Notify


specLiveNode = do
  specSimpleLiveNode
  specSimpleInterp

specSimpleLiveNode = do
  describe "simple live node" $ do
    it "compose the child nodes correctly" $ do
      x <- recvIO . toProducer $ testSimpleLive2 (return testSimpleLive)
      y <- recvIO $ toProducer testSimpleLive2Res
      x `shouldBe` y

specSimpleInterp = do
  describe "simple string interpolation" $ do
    it "should isnert the string in the quasquoted template" $ do
      x <- recvIO $ toProducer testSimpleInsert
      y <- recvIO $ toProducer testSimpleInsertRes
      x `shouldBe` y

testSimpleLive :: LiveVDom
testSimpleLive = [gertrude|
<some other="test">
  <with a="child">
|]

testSimpleLive2 :: STMEnvelope LiveVDom -> LiveVDom
testSimpleLive2 inp = [gertrude|
<some header="value">
  <with children="nodes">
    !{id . id . id $ inp}
  <and some="other values">
|]

testSimpleLive2Res :: LiveVDom
testSimpleLive2Res = [gertrude|
<some header="value">
  <with children="nodes">
    <some other="test">
      <with a="child">
  <and some="other values">
|]

testSimpleInsert :: LiveVDom
testSimpleInsert = let val = 4 in [gertrude|
<some header="value">
  <with children="nodes">
    <some other="test">
      #{show 4}
  <and some="other values">
|]

testSimpleInsertRes :: LiveVDom
testSimpleInsertRes = [gertrude|
<some header="value">
  <with children="nodes">
    <some other="test">
      4
  <and some="other values">
|]
