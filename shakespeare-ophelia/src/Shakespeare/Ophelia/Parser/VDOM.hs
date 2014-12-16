{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shakespeare.Ophelia.Parser.VDOM where


import           BasicPrelude               hiding (foldl, try)

import           VDOM.Adapter

import qualified Data.Foldable              as F
import           Data.String.Here
import           Data.Text                  hiding (length, null)

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Trifecta.Delta
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import qualified Data.Traversable           as T
import qualified Data.Tree                  as T

import           Shakespeare.Ophelia.Parser

-- parseVNodeS :: (Monad m, Functor m) => String -> Result (Result [VNodeAdapter])
-- parseVNodeS x = do -- (fromTree parseVNodeAdapter) <$> (parseString parseLineForest (Columns 0 0) x)
--   let parsed = parseString parseLineForest (Columns 0 0)
--   fromTree parseVNodeAdapter

parseVNodeS :: (Applicative f, Monad f) => String -> f (Result [VNodeAdapter])
parseVNodeS = parseStringTrees parseVNodeAdapter



parseVNodeAdapter :: (Monad m) => Parser ([VNodeAdapter] -> m VNodeAdapter)
parseVNodeAdapter = (parseVNode) <|> (parseVText)

parseVNode :: (Monad m) => Parser ([VNodeAdapter] -> m VNodeAdapter)
parseVNode = angles $ do
  tagName <- manyTill alphaNum (space <|> (lookAhead $ char '>'))
  props <- many parseAttribute
  (return $ \children -> return $ VNode tagName props children) <?> "VNode"

parseVText :: (Monad m) => Parser ([VNodeAdapter] -> m VNodeAdapter)
parseVText = do
  xs <- many anyChar
  (return $ \vns -> F.foldlM addVText (VText xs) vns) <?> "VText"
  where addVText (VText accum) (VText new) = return $ VText $ accum ++ "\n" ++ new
        addVText _ vn@(VNode _ _ _) = fail [i| Unable to add node ${show vn} to text as a node|]

test = [here|
<div test="5">
  <achild ishere="equals5" anotherProp="Hello">
    Some child text is here
      while some more child text
    but there is some child text here too
    <this should="fail" doesThis="Work?" ithink="4">
|]

parseAttribute :: Parser Property
parseAttribute = do
  _ <- spaces
  name <- manyTill (noneOf [' ', '>']) $ char '='
  val <- parseJSProp
  if null name 
    then fail "Unable to have empty proprty value"
    else return $ Property name val

parseJSProp :: Parser JSProp
parseJSProp =  parseJSPInt <|> parseJSPDouble <|> (JSPText <$> stringLiteral) <|> (JSPText <$> stringLiteral')

parseJSPInt :: Parser JSProp
parseJSPInt = JSPInt . fromIntegral <$> quoted integer

parseJSPDouble :: Parser JSProp
parseJSPDouble = JSPDouble <$> quoted double

quoted :: Parser a -> Parser a
quoted nParser = try $ (between (char '"') (char '"') nParser)



-- combineVText :: VNodeAdapter -> VNodeAdapter
-- combineVText VNodeAdapter
