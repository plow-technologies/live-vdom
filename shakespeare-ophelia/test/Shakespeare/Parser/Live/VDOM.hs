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
  specSimpleInterp

specSimpleLiveNode = do
  describe "simple live node" $ do
    it "compose the child nodes correctly" $ do
      x <- atomically . recv . toProducer $ testSimpleLive2 (return testSimpleLive)
      y <- atomically . recv $ toProducer testSimpleLive2Res 
      x `shouldBe` y

specSimpleInterp = do
  describe "simple string interpolation" $ do
    it "should isnert the string in the quasquoted template" $ do
      x <- atomically . recv $ toProducer  testSimpleInsert
      y <- atomically . recv $ toProducer  testSimpleInsertRes
      x `shouldBe` y


testSimpleLive = [gertrude|
<some other="test">
  <with a="child">
|]


testSimpleLive2 inp = [gertrude|
<some header="value">
  <with children="nodes">
    !{id inp}
  <and some="other values">
|]


testSimpleLive2Res = [gertrude|
<some header="value">
  <with children="nodes">
    <some other="test">
      <with a="child">
  <and some="other values">
|]

testSimpleInsert = let val = 4 in [gertrude|
<some header="value">
  <with children="nodes">
    <some other="test">
      #{show val}
  <and some="other values">
|]


testSimpleInsertRes = [gertrude|
<some header="value">
  <with children="nodes">
    <some other="test">
      4
  <and some="other values">
|]