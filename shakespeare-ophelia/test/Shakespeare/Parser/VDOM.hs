{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Shakespeare.Parser.VDOM where

import           BasicPrelude

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
import           VDOM.Adapter


specParser :: Spec
specParser = do
  specSimpleProp
  specMultiProp
  specSimpleVText
  specSimpleVNode
  specBrokenVNodeInVText
  specSimpleNestedVNode

pStringTree :: String -> Maybe (Result [VNodeAdapter])
pStringTree = parseVNodeS

list x = [x]

parseStringNode :: (Monad m) => String -> m [VNodeAdapter]
parseStringNode st = do
  let x = pStringTree st
  case x of
    Nothing -> fail $ unableToParse st
    Just resParse -> explainResult resParse
  where unableToParse str = "Unable to parse: " ++ str


explainParse :: (Monad m) => Parser a -> String -> m a
explainParse parser str = do
  case parseString parser (D.Columns 0 0) str of
    Success res -> return res
    Failure doc -> fail . T.unpack $ show doc

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

isFailure :: Result a -> Bool
isFailure = not . isSuccess

explainResult :: (Monad m) => Result a -> m a
explainResult (Success res) = return res
explainResult (Failure doc) = fail . T.unpack $ show doc

specSimpleProp = do
  describe "simple property" $ do
    it "should parse a simple property" $ do
      res <- explainParse parseAttribute specSimplePropString
      res `shouldBe` specSimplePropProp


specMultiProp = do
  describe "multi-prop parsing" $ do
    it "should parse all property types" $ do
      props <- explainParse propsParser specMultiPropString
      props `shouldBe` specMultiPropProp
  where propsParser = many parseAttribute


specSimpleVText = do
  describe "simple vtext" $ do
    it "should parse a simple vtext" $ do
      tree <- parseStringNode specSimpleVTextString
      tree `shouldBe`  (list specSimpleVTextNode)


specSimpleVNode = do
  describe "simple vnode" $ do
    it "should parse a simple vnode with properties" $ do
      vnode <- parseStringNode specSimpleVNodeString
      vnode `shouldBe` (list specSimpleVNodeNode)

specBrokenVNodeInVText = do
  describe "vnode inside vtext" $ do
    it "should fail to parse" $ do
      let failed = pStringTree specBrokenVNodeInVTextString
      case failed of
        Nothing -> True `shouldBe` True
        _ -> True `shouldBe` False

specSimpleNestedVNode = do
  describe "nest vnodes and vtext" $ do
    it "should parse a simple nested VNodeAdapter" $ do
      vn <- parseStringNode specSimpleNestedVNodeString
      vn `shouldBe` (list specSimpleNestedVNodeNode)


specSimplePropString :: String
specSimplePropString = [here|hello="world"|]

specSimplePropProp :: Property
specSimplePropProp = Property "hello" $ JSPText "world"

specMultiPropString :: String
specMultiPropString = [here|hello="world" width="100" somefloat="10.5" correction="123.5" fill="full"|]

specMultiPropProp :: [Property]
specMultiPropProp = [
                      Property "hello" $ JSPText "world"
                    , Property "width" $ JSPInt 100
                    , Property "somefloat" $ JSPDouble 10.5
                    , Property "correction" $ JSPDouble 123.5
                    , Property "fill" $ JSPText "full"
                    ]

specSimpleVTextString :: String
specSimpleVTextString = [here|This is a simple VText|]

specSimpleVTextNode :: VNodeAdapter
specSimpleVTextNode = VText specSimpleVTextString

specSimpleVNodeString :: String
specSimpleVNodeString = [here|<div someProp="someString" where="somefloat" is="15.6">|]

specSimpleVNodeNode :: VNodeAdapter
specSimpleVNodeNode = VNode "div" props []
  where props = [
                  Property "someProp" $ JSPText "someString"
                , Property "where" $ JSPText "somefloat"
                , Property "is" $ JSPDouble 15.6
                ]

specBrokenVNodeInVTextString :: String
specBrokenVNodeInVTextString = [here|
SomeVText
  <div thisisbad="fail">
|]

specSimpleNestedVNodeString :: String
specSimpleNestedVNodeString = [here|
<div property="string" class="style or something" width="100%" value="10">
  <a href="http://google.com">
    Link text!
  <table>
    <tr>
      <th>
        Month
      <th>
        Savings
    <tr>
      <td ng-repeat="x in xs">
        x
|]

specSimpleNestedVNodeNode :: VNodeAdapter
specSimpleNestedVNodeNode = VNode "div" divProps children
  where children = [link,table]
        link = VNode "a" [Property "href" $ JSPText "http://google.com"] [VText "Link text!"]
        table = VNode "table" []  [
                                    VNode "tr" [] [
                                                    VNode "th" [] [VText "Month"]
                                                  , VNode "th" [] [VText "Savings"]
                                                  ]
                                  , VNode "tr" [] [
                                                    VNode "td" [Property "ng-repeat" $ JSPText "x in xs"] [VText "x"]
                                                  ]
                                  ]
        divProps = [
                      Property "property" $ JSPText "string"
                    , Property "class" $ JSPText "style or something"
                    , Property "width" $ JSPText "100%"
                    , Property "value" $ JSPInt 10
                   ]