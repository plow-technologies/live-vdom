{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia.Parser.Live.VDOM where


import           Prelude                               hiding (foldl,foldr)


import           Control.Applicative
import           Data.Foldable
import           Data.List.Split
import           Data.String.Here

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Trifecta.Parser

import           Shakespeare.Ophelia.Parser.Live.Types
import           Shakespeare.Ophelia.Parser.VDOM

import           Language.Haskell.TH


parsePLiveVDom :: (Monad m) => Parser ([PLiveVDom] -> m PLiveVDom)
parsePLiveVDom = parseStaticNode <|> parseLiveVNode <|> parseStaticText


parseStaticNode :: (Monad m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseStaticNode = angles $ do
  tagName <- parseTagName       -- Reuse the parsers in the static vdom file
  props <- many parseAttribute
  (return $ \children -> return $ PLiveVNode tagName props children) <?> "LiveVNode"

buildF :: String -> Exp
buildF str = foldl (\e app -> AppE e (VarE $ mkName app)) (VarE . mkName . head $ xs) $ tail xs
  where xs = splitOn " " str

removeTogether :: (Eq a) =>  a -> [a] -> [a]
removeTogether a str = foldr go [] str
  where go y [] = [y]
        go y (x:xs) = if x == a && y == a
                        then x:xs
                        else y:x:xs

parseLiveVNode :: (Monad m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseLiveVNode = do
  char '!'
  braces $ do
    expr <- manyTill anyChar $ lookAhead $ char '}'
    let failOnNonempty [] = return . PLiveChild . buildF $ (removeTogether ' ' expr)
        failOnNonempty _ = fail "Error. Live nodes are unable to have children"
    return failOnNonempty


parseStaticText :: (Monad m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseStaticText = do
  xs <- many anyChar
  (return $ \vns -> foldlM addPVText (PLiveVText xs) vns) <?> "VText"
  where addPVText (PLiveVText accum) (PLiveVText new) = return $ PLiveVText $ accum ++ "\n" ++ new
        addPVText _ (PLiveVNode _ _ _) = fail [here| Unable to add node to text as a node|]
        addPVText _ _ = fail [i|Error, somehow parsed VNode instead of PLiveVText. Please report this as a bug|]


stupidTest :: String
stupidTest = [here|
<some text="something">
  <with some="other stuff">
  !{undefined}
  <it should="atleast">
    Successfully parse!
|]