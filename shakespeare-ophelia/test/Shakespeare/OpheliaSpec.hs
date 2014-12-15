{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
module Shakespeare.OpheliaSpec (main, spec) where

import           BasicPrelude

import qualified Text.Trifecta.Delta  as D
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Test.Hspec

import           Data.String.Here
import           Shakespeare.Ophelia
import qualified Data.Text as T


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specSimpleParse
  specConvexTree
  specConcavePortions

specSimpleParse :: Spec
specSimpleParse = do
  describe "simple parsing" $ do
    it "should parse a simple structure" $ do
      specParseForrest simpleParseString simpleParseLines

specConvexTree :: Spec
specConvexTree = do
  describe "'Convex tree' parsing" $ do
    it "Should parse a tree with a maximum of two children for each node" $ do
      specParseForrest convexLinesString convexLines

specConcavePortions :: Spec
specConcavePortions = do
  describe "Parsing multiple convex sections" $ do
    it "should parse children in the correct order and level" $ do
      specParseForrest concaveLinesString concaveLines

specParse :: (Show a, Eq a) => Parser a -> String -> a -> Expectation
specParse parser input expectedResult = do
  let rParsed = parseString parser (D.Columns 0 0) input
  case rParsed of
    Success parsed -> parsed `shouldBe` expectedResult
    Failure d -> fail . T.unpack $ show d

specParseForrest :: String -> [Lines] -> Expectation
specParseForrest = specParse parseLineForrest




simpleParseString :: String
simpleParseString = [here|
Parent One
  Child of Parent One 1
    Child of Child of Parent One
    Child of Parent One 2
Parent Two
  Child of Parent Two

|]

simpleParseLines :: [Lines]
simpleParseLines = [
                      Lines "Parent One" 0
                        [
                          Lines "Child of Parent One 1" 2
                            [
                              Lines "Child of Child of Parent One" 4 []
                            , Lines "Child of Parent One 2" 4 []
                            ]
                        ]
                    , Lines "Parent Two" 0
                        [Lines "Child of Parent Two" 2 []]
                    ]




convexLinesString :: String
convexLinesString = [here|
Parent One
  Child One
    Child Two
      Child Three
        Child Four
          Child Five
        Child Six
      Child Seven
    Child Eight
  Child Nine
Parent Two
|]

convexLines :: [Lines]
convexLines = [
    Lines "Parent One" 0 [
      Lines "Child One" 2 [
        Lines "Child Two" 4 [
          Lines "Child Three" 6 [
            Lines "Child Four" 8 [
              Lines "Child Five" 10 []
            ]
           ,Lines "Child Six" 8 []
          ]
        ,Lines "Child Seven" 6 []
        ]
      ,Lines "Child Eight" 4 []
      ]
    ,Lines "Child Nine" 2 []
    ]
   ,Lines "Parent Two" 0 []
  ]

concaveLinesString :: String
concaveLinesString = [here|
Parent One
  Child One
    Child Two
    Child Three
      Child Four
        Child Five
          Child Six
        Child Seven
      Child Eight
    Child Nine
    Child Ten
    Child Eleven
  Child Twelve
    Child Thirteen
      Child Fourteen
|]

concaveLines :: [Lines]
concaveLines = [
    Lines "Parent One" 0 [
      Lines "Child One" 2 [
        Lines "Child Two" 4 []
       ,Lines "Child Three" 4 [
          Lines "Child Four" 6 [
            Lines "Child Five" 8 [
              Lines "Child Six" 10 []
            ]
           ,Lines "Child Seven" 8 []
          ]
       ,Lines "Child Eight" 6 []
        ]
      ,Lines "Child Nine" 4 []
      ,Lines "Child Ten" 4 []
      ,Lines "Child Eleven" 4 []
      ]
      ,Lines "Child Twelve" 2 [
        Lines "Child Thirteen" 4 [
          Lines "Child Fourteen" 6 []
        ]
      ]
    ]
  ]