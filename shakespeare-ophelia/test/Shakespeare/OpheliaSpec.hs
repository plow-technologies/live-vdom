{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
module Shakespeare.OpheliaSpec  where

import           BasicPrelude

import qualified Text.Trifecta.Delta  as D
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Test.Hspec

import           Data.String.Here
import qualified Data.Text            as T
import qualified Data.Tree            as T
import           Shakespeare.Ophelia


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specSimpleParse
  specConvexTree
  specConcavePortions
  specConcaveConcatenation
  specMultiLevels

specSimpleParse :: Spec
specSimpleParse = do
  describe "simple parsing" $ do
    it "should parse a simple structure" $ do
      specParseForest simpleParseString simpleParseLines

specConvexTree :: Spec
specConvexTree = do
  describe "'Convex tree' parsing" $ do
    it "Should parse a tree with a maximum of two children for each node" $ do
      specParseForest convexLinesString convexLines

specConcavePortions :: Spec
specConcavePortions = do
  describe "Parsing multiple convex sections" $ do
    it "should parse children in the correct order and level" $ do
      specParseForest concaveLinesString concaveLines

specConcaveConcatenation :: Spec
specConcaveConcatenation = do
  describe "concatenating strings should concatenate forests" $ do
    it "should hand the concatenation of forests" $ do
      specParseForest (repeat10 concaveLinesString) (concat $ replicate 10 concaveLines)
  where repeat10 xs = (intercalate "\n") $ replicate 10 xs

specMultiLevels :: Spec
specMultiLevels = do
  describe "Multiple space tree" $ do
    it "Should parse a tree that uses both 2 and 3 spaces" $ do
      specParseForest multiLevelsString multiLevels

specParse :: (Show a, Eq a) => Parser a -> String -> a -> Expectation
specParse parser input expectedResult = do
  let rParsed = parseString parser (D.Columns 0 0) input
  case rParsed of
    Success parsed -> parsed `shouldBe` expectedResult
    Failure d -> fail . T.unpack $ show d

specParseForest :: String -> T.Forest String -> Expectation
specParseForest st t = specParse parseLineForest st (ParsedTree t)




simpleParseString :: String
simpleParseString = [here|
Parent One
  Child of Parent One 1
    Child of Child of Parent One
    Child of Parent One 2
Parent Two
  Child of Parent Two

|]

simpleParseLines :: T.Forest String
simpleParseLines = [
                      T.Node "Parent One"
                        [
                          T.Node "Child of Parent One 1"
                            [
                              T.Node "Child of Child of Parent One"[]
                            , T.Node "Child of Parent One 2" []
                            ]
                        ]
                    , T.Node "Parent Two"
                        [T.Node "Child of Parent Two" []]
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


convexLines :: T.Forest String
convexLines = [
    T.Node "Parent One" [
      T.Node "Child One" [
        T.Node "Child Two" [
          T.Node "Child Three" [
            T.Node "Child Four" [
              T.Node "Child Five" []
            ]
           ,T.Node "Child Six" []
          ]
        ,T.Node "Child Seven" []
        ]
      ,T.Node "Child Eight" []
      ]
    ,T.Node "Child Nine" []
    ]
   ,T.Node "Parent Two" []
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

concaveLines :: T.Forest String
concaveLines = [
    T.Node "Parent One" [
      T.Node "Child One" [
        T.Node "Child Two" []
       ,T.Node "Child Three" [
          T.Node "Child Four" [
            T.Node "Child Five" [
              T.Node "Child Six" []
            ]
           ,T.Node "Child Seven" []
          ]
       ,T.Node "Child Eight" []
        ]
      ,T.Node "Child Nine" []
      ,T.Node "Child Ten" []
      ,T.Node "Child Eleven" []
      ]
      ,T.Node "Child Twelve" [
        T.Node "Child Thirteen" [
          T.Node "Child Fourteen" []
        ]
      ]
    ]
  ]

multiLevelsString :: String
multiLevelsString = [here|
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
Parent Two
|]

multiLevels :: T.Forest String
multiLevels = [
    T.Node "Parent One" [
      T.Node "Child One" [
        T.Node "Child Two" []
       ,T.Node "Child Three" [
          T.Node "Child Four" []
       ]
      ]
     ,T.Node "Child Five" [
        T.Node "Child Six" [
          T.Node "Child Seven" [
            T.Node "Child Eight" []
          , T.Node "Child Nine" []
          ]
         ,T.Node "Child Ten" []
        ]
      ]
    ]
  , T.Node "Parent Two" []
  ]