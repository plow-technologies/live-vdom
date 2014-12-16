{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
module Shakespeare.Parser  where

import           BasicPrelude

import qualified Text.Trifecta.Delta  as D
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Test.Hspec

import           Data.String.Here
import qualified Data.Text            as T
import qualified Data.Tree            as T
import           Shakespeare.Ophelia.Parser


-- specParser :: Spec
-- specParser = do
--   describe "should do something" $ do
--     it "should be" $ do
--       True `shouldBe` True


-- specSimpleProp = do
--   describe "simple property" $ do
--     it "should parse a simple property" $ do
      

-- specSimpleVText = undefined

-- specSimpleVNode = undefined

-- specBrokenVNodeInVText = undefined

-- specSimpleNestedVNode = undefined

-- specNestedVNode = undefined

-- specCombiningVNodes = undefined

-- specConvexVNode = undefined