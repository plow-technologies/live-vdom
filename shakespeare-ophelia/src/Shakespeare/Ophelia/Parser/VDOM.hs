{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia.Parser.VDOM where


import           Prelude                    hiding (foldl)

import           VDOM.Adapter

import           Control.Applicative
import qualified Data.Foldable              as F
import           Data.String.Here

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Shakespeare.Ophelia.Parser


-- | Parse a VNode string in the form of:
-- <div property="some string" or="4">
--    <p with="child nodes">
--      And text!
parseVNodeS :: (Applicative f, Monad f) => String -> f (Result [VNodeAdapter])
parseVNodeS = parseStringTrees parseVNodeAdapter

-- | The function that's used to parse a string tree
-- This is how the vnode parser is seperated
-- because it parsed a vnode and then tries to parse vtext
parseVNodeAdapter :: (Monad m) => Parser ([VNodeAdapter] -> m VNodeAdapter)
parseVNodeAdapter = parseVNode <|> parseVText


-- | Parse a single property and then give a function to add
-- a number of chid vnodes. The operation is monadic because the vtext portion
-- can fail
parseVNode :: (Monad m) => Parser ([VNodeAdapter] -> m VNodeAdapter)
parseVNode = angles $ do
  tagName <- parseTagName
  props <- many parseAttribute
  (return $ \children -> return $ VNode [] tagName props children) <?> "VNode"


parseTagName :: Parser String
parseTagName = manyTill alphaNum (space <|> (lookAhead $ char '>'))


-- | Parse a single text element
-- If any VNode is passed in as a child the parser fails
parseVText :: (Monad m) => Parser ([VNodeAdapter] -> m VNodeAdapter)
parseVText = do
  xs <- many anyChar
  (return $ \vns -> F.foldlM addVText (VText [] xs) vns) <?> "VText"
  where addVText (VText _ accum) (VText _ new) = return $ VText [] $ accum ++ "\n" ++ new
        addVText _ (VNode _ev tn props _ch) = fail [i| Unable to add node ${show tn ++ " " ++ show props} to text as a node|]
        addVText _ _ = fail [i|Error, somehow parsed VNode instead of VText. Please report this as a bug|]

-- | Consumes leading spaces, the name of the property
-- then equals, and then the JSProp
parseAttribute :: Parser Property
parseAttribute = do
  _ <- spaces
  name <- manyTill (noneOf [' ', '>']) $ char '='
  val <- parseJSProp
  if null name
    then fail "Unable to have empty proprty value"
    else return $ Property name val

-- | Attemtps to parse int -> double -> strings -> single quoted strings
parseJSProp :: Parser JSProp
parseJSProp =  parseJSPInt <|> parseJSPDouble <|> (JSPText <$> stringLiteral) <|> (JSPText <$> stringLiteral')

-- | A quoted integer
parseJSPInt :: Parser JSProp
parseJSPInt = JSPInt . fromIntegral <$> quoted integer

-- | A quoted double
parseJSPDouble :: Parser JSProp
parseJSPDouble = JSPDouble <$> quoted double

quoted :: Parser a -> Parser a
quoted nParser = try $ (between (char '"') (char '"') nParser)