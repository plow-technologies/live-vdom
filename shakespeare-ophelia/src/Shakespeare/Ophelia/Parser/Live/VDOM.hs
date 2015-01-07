{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia.Parser.Live.VDOM where


import           Prelude                               hiding (foldl)


import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.List.Split
import           Data.String.Here

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Shakespeare.Ophelia.Parser
import           Shakespeare.Ophelia.Parser.Live.Types
import           Shakespeare.Ophelia.Parser.VDOM

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax


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

parseLiveVNode :: (Monad m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseLiveVNode = do
  char '!'
  braces $ do
    expr <- manyTill anyChar $ lookAhead $ char '}'
    return . const . return . PLiveChild . buildF $ expr


parseStaticText :: (Monad m) => Parser ([PLiveVDom] -> m PLiveVDom)
parseStaticText = do
  xs <- many anyChar
  (return $ \vns -> foldlM addPVText (PLiveVText xs) vns) <?> "VText"
  where addPVText (PLiveVText accum) (PLiveVText new) = return $ PLiveVText $ accum ++ "\n" ++ new
        addPVText _ vn@(PLiveVNode _ _ _) = fail [i| Unable to add node to text as a node|]
        addPVText _ _ = fail [i|Error, somehow parsed VNode instead of PLiveVText. Please report this as a bug|]

