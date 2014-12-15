{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia.Parser.VDOM where


import           BasicPrelude hiding (foldl, try)

import           VDOM.Adapter

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta.Parser
import           Text.Trifecta.Delta
import           Text.Trifecta.Result
import Text.Parser.Token
import Data.Text
import Data.String.Here


import Shakespeare.Ophelia.Parser

parseVNodeS :: String -> Result (Result [VNodeAdapter])
parseVNodeS x = (fromTree parseVNode) <$> (parseString parseLineForest (Columns 0 0) x)

-- parseVNode :: Parser VNodeAdapter
parseVNode :: Parser ([VNodeAdapter] -> VNodeAdapter)
parseVNode = angles $ do
  tagName <- manyTill alphaNum space
  props <- many parseAttribute
  return $ VNode tagName props

test = [here|
<div test="5">
  <achild ishere="equals5">
|]

parseAttribute :: Parser Property
parseAttribute = do
  name <- manyTill alphaNum $ char '='
  val <- parseJSProp
  return $ Property name val

parseJSProp :: Parser JSProp
parseJSProp =  parseJSPInt <|> parseJSPDouble <|> (JSPText <$> stringLiteral) <|> (JSPText <$> stringLiteral')

parseJSPInt :: Parser JSProp
parseJSPInt = JSPInt . fromIntegral <$> quoted integer

parseJSPDouble :: Parser JSProp
parseJSPDouble = JSPDouble <$> quoted double

quoted :: Parser a -> Parser a
quoted nParser = try $ (between (char '"') (char '"') nParser)