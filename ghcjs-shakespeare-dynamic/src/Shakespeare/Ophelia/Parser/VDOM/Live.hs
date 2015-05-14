{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia.Parser.VDOM.Live where


import           Prelude                               hiding (foldl, foldr)


import           Control.Applicative
import           Data.Foldable
import           Data.String.Here

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Trifecta.Delta
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Shakespeare.Ophelia.Parser.VDOM
import           Shakespeare.Ophelia.Parser.VDOM.Types

import           Language.Haskell.TH

import           Shakespeare.Ophelia.Parser

import           Language.Haskell.Meta.Parse

import qualified Data.Sequence as S

parseLiveDom :: (Applicative f, Monad f) => String -> f (Result [PLiveVDom])
parseLiveDom = parseStringTrees parsePLiveVDom

parsePLiveVDom :: (Monad m, Functor m) => Parser ([PLiveVDom] -> m PLiveVDom)
parsePLiveVDom = parseStaticNode <|> parseLiveVNode <|> parseMultipleLiveNodes <|> parseLiveText <|> parseStaticText


parseStaticNode :: (Monad m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseStaticNode = angles $ do
  tagName <- parseTagName       -- Reuse the parsers in the static vdom file
  props <- many parseAttribute
  (return $ \children -> return $ PLiveVNode tagName props children) <?> "LiveVNode"

buildF :: (Monad m, Functor m) => String -> m Exp
buildF str = case parseExp str of
               (Left e) -> fail e
               (Right e) -> return e

parseLitExpr :: String -> Exp
parseLitExpr str = fromResult (VarE $ mkName str) $ LitE <$> parseString parseHLit (Columns 0 0) str

fromResult :: a -> Result a -> a
fromResult _ (Success s) = s
fromResult s _ = s

parseHLit :: Parser Lit
parseHLit = (CharL <$> charLiteral) <|> (StringL <$> stringLiteral)
        <|> (IntegerL <$> integer) <|> (DoublePrimL . toRational <$> double)


removeTogether :: (Eq a) =>  a -> [a] -> [a]
removeTogether a str = foldr go [] str
  where go y [] = [y]
        go y (x:xs) = if x == a && y == a
                        then x:xs
                        else y:x:xs

parseLiveVNode :: (Monad m, Functor m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseLiveVNode = do
  _ <- char '!'
  braces $ do
    expr <- manyTill anyChar $ lookAhead $ char '}'
    let failOnNonempty [] = PLiveChild <$> buildF expr
        failOnNonempty _ = fail "Error. Live nodes are unable to have children"
    return failOnNonempty

parseMultipleLiveNodes :: (Monad m, Functor m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseMultipleLiveNodes = do
  _ <- char '&'
  braces $ do
    expr <- manyTill anyChar $ lookAhead $ char '}'
    let failOnNonempty [] = PLiveChildren <$> buildF expr
        failOnNonempty _ = fail "Error. Live nodes are unable to have children"
    return failOnNonempty


parseLiveText :: (Monad m, Functor m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseLiveText = do
  _ <- char '#'
  braces $ do
    expr <- manyTill anyChar $ lookAhead $ char '}'
    let failOnNonempty [] = PLiveInterpText <$> buildF expr
        failOnNonempty _ = fail "Error. Live nodes are unable to have children"
    return failOnNonempty

parseStaticText :: (Monad m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseStaticText = do
  xs <- many anyChar
  (return $ \vns -> foldlM addPVText (PLiveVText xs) vns) <?> "VText"
  where addPVText (PLiveVText accum) (PLiveVText new) = return $ PLiveVText $ accum ++ "\n" ++ new
        addPVText _ (PLiveVNode _ _ _) = fail [here| Unable to add node to text as a node|]
        addPVText _ _ = fail [i|Error, somehow parsed VNode instead of PLiveVText. Please report this as a bug|]
