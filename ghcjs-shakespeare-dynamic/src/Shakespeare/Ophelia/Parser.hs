{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shakespeare.Ophelia.Parser (
  parseLineForest
, ParsedTree(..)
, fromTree
, parseStringTrees
)
where

import           Prelude                 hiding (foldl, sequence)


import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import qualified Data.Tree               as T
import qualified Data.Traversable        as T


import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta.Delta
import           Text.Trifecta.Parser
import           Text.Trifecta.Result


type PrepositionTree a = T.Tree (Int,a)
newtype ParsedTree a = ParsedTree { unParsedTree :: [T.Tree a] } deriving (Eq, Show)



-- | Because Result isn't an instance of monad but it's
-- needed when you parse something tice
joinR :: (Result (Result a)) -> Result a
joinR (Failure doc) = Failure doc
joinR (Success d) = d


-- | Given a parser for a tree-like structure, parse a string
parseStringTrees :: (Applicative f, Monad f) => Parser ([a] -> f a) -- ^ Parse the root of a tree and then allow for a list of children to be added
                                                -> String  -- ^ String to parse
                                                -> f (Result [a])
parseStringTrees builder str = do
  let parsedTrees = parseString (spaces >> parseLineForest) (Columns 0 0) str -- parse all spaces and then a line forrest
  joinR <$> (T.sequenceA $ (fromTree builder) <$> parsedTrees)


fromTree :: (Monad m, Functor m) => Parser ([a] -> m a) -> ParsedTree String -> m (Result [a])
fromTree builder (ParsedTree xs) = T.sequenceA <$> T.mapM (fromTree' builder) xs

fromTree' :: (Monad m, Functor m) => Parser ([a] -> m a) -> T.Tree String -> m (Result a)
fromTree' builder (T.Node st ch) = do
  let buildT = parseString builder (Columns 0 0) st
  ch' <- T.sequenceA <$> T.forM ch (fromTree' builder)
  T.sequence $ buildT <*> ch'



-- | Parse a set of strings based on their indentation and form a tree
-- structure
parseLineForest :: Parser (ParsedTree String)
parseLineForest = do
  xs <- parseLines
  let xs' = foldl insertAtLevel [] (toPrepostionTree <$> (filter (\(_,b) -> b /= "") xs)) -- Insert at the tree the text that doesn't contain an empty line
  return $ toParsedTree xs'

-- Insert a tree into the current tree structure
insertAtLevel :: [PrepositionTree a] -- ^ Current tree structure
                -> PrepositionTree a -- ^ Tree to insert
                -> [PrepositionTree a]
insertAtLevel [] new = [new]
insertAtLevel (old:xs) new = case compare newLevel oldLevel of
                              GT -> inserted:xs
                              _ -> new:old:xs
  where newLevel = currentLevel new
        oldLevel = currentLevel old
        currentLevel = fst . T.rootLabel
        inserted = addInsert old new
        addInsert (T.Node label ch) newI = T.Node label $ insertAtLevel ch newI

-- | Count the number of spaces preceding a character other than a space
spacesCount :: Parser Int
spacesCount = length <$> many (char ' ')

-- | Return the number of spaces leading up to a given string
-- "   a" -> (3,"a"), "abc" -> (0,"abc")
parseLine :: Parser (Int,String)
parseLine = do
  whiteSpaceCount <- spacesCount
  line <- manyTill anyChar eofNewLine
  return (whiteSpaceCount, line)

-- | Parse a group of lines with their respective number of leading whitespaces
parseLines :: Parser [(Int,String)]
parseLines = manyTill parseLine eofNewLine

-- | Make an empty prepositioned tree
toPrepostionTree :: (Int,String) -> PrepositionTree String
toPrepostionTree = flip T.Node []

-- | Because of the foldl of the insert you have to reverse all of the children for a given  node
reverseChildren :: PrepositionTree a -> PrepositionTree a
reverseChildren (T.Node nodeLabel nodeChildren) = T.Node nodeLabel (reverse (reverseChildren <$> nodeChildren))

-- | Disregard the the whitespaces and reverse the children
toTree :: PrepositionTree a -> T.Tree a
toTree prepTree = snd <$> (reverseChildren prepTree)

toParsedTree :: [PrepositionTree a] -> ParsedTree a
toParsedTree = ParsedTree . reverse . (fmap toTree)

-- | Parse an eof OR newline
eofNewLine :: Parser ()
eofNewLine = (eof <|> (void newline))
