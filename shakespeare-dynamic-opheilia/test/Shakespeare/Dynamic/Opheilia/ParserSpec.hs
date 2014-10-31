{-# LANGUAGE QuasiQuotes #-}

module Shakespeare.Dynamic.Opheilia.ParserSpec (main, spec) where

import Shakespeare.Dynamic.Opheilia.Parser
import Test.Hspec
import Data.String.Here.Uninterpolated
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do     
      (show testParseDoc) `shouldBe` "Ok True"



exampleString :: [Doc]
exampleString = read "[DocContent (ContentRaw \"<div class=\\\"span12\\\" ng-repeat=\\\"fakes\\\">                        <h1> Here I come again</h1>\\n<h2> on my own</h2>\\n<h3> we are gonna make it<i> whee </i> </i>\\n</h3>\\n</div>\\n<div> oh, we aint gonna make it   </div>\\n\")]"
testParseDoc = do
  (_,docs) <- parseDoc defaultHamletSettings exampleHamletString  
  return (docs == exampleString)

exampleHamletString = [hereLit|
<div class="span12" ng-repeat="fakes">                        
 <h1> Here I come again
 <h2> on my own
 <h3> we are gonna make it
  <i> whee </i> 
<div> oh, we aint gonna make it   
|]                       
