{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia.Parser.VDOM where


import           BasicPrelude hiding (foldl, try)

import           VDOM.Adapter

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta.Parser
import Text.Parser.Token
import Data.Text


parseVNode :: Parser VNodeAdapter
parseVNode = angles $ do
  tagName <- manyTill alphaNum space
  props <- many parseAttribute
  return $ VNode tagName props []


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