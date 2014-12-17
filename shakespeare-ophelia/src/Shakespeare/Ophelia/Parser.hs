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


import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import qualified Data.Tree               as T
import           Prelude                 hiding (foldl, sequence)


import           Data.Traversable
import qualified Data.Traversable        as T
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta.Delta
import           Text.Trifecta.Parser
import           Text.Trifecta.Result


type PrepositionTree a = T.Tree (Int,a)
newtype ParsedTree a = ParsedTree { unParsedTree :: [T.Tree a] } deriving (Eq, Show)




joinR :: (Result (Result a)) -> Result a
joinR (Failure doc) = Failure doc
joinR (Success d) = d

parseStringTrees :: (Applicative f, Monad f) =>Parser ([a] -> f a) -> String -> f (Result [a])
parseStringTrees builder str = do
  let parsedTrees = parseString parseLineForest (Columns 0 0) str -- Result (ParsedTree String)
  joinR <$> (T.sequenceA $ (fromTree builder) <$> parsedTrees)


fromTree :: (Monad m, Functor m) => Parser ([a] -> m a) -> ParsedTree String -> m (Result [a])
fromTree builder (ParsedTree xs) = do
  trees <- T.mapM (fromTree' builder) xs
  return $ sequenceA trees

fromTree' :: (Monad m, Functor m) => Parser ([a] -> m a) -> T.Tree String -> m (Result a)
fromTree' builder (T.Node st ch) = do
  let buildT = parseString builder (Columns 0 0) st
  ch' <- T.sequenceA <$> T.forM ch (fromTree' builder)
  T.sequence $ buildT <*> ch'



-- | Parse a string to a parsed tree. This is a whitespaces parsed tree
parseLineForest :: Parser (ParsedTree String)
parseLineForest = do
  xs <- parseLines
  let xs' = foldl insertAtLevel [] (toPrepostionTree <$> xs)
  return $ toParsedTree xs'

-- Insert a tree into
insertAtLevel :: [PrepositionTree a] -> PrepositionTree a -> [PrepositionTree a]
insertAtLevel [] new = [new]
insertAtLevel (old:xs) new = case compare newLevel oldLevel of
                              GT -> inserted:xs
                              _ -> new:old:xs
  where newLevel = currentLevel new
        oldLevel = currentLevel old
        currentLevel = fst . T.rootLabel
        inserted = addInsert old new
        addInsert (T.Node label ch) newI = T.Node label $ insertAtLevel ch newI

-- Count the number of spaces preceding a character other than a space
spacesCount :: Parser Int
spacesCount = length <$> many (char ' ')

-- Return the number of spaces leading up to a given string
-- "   a" -> (3,"a"), "abc" -> (0,"abc")
parseLine :: Parser (Int,String)
parseLine = do
  whiteSpaceCount <- spacesCount
  line <- manyTill anyChar eofNewLine
  return (whiteSpaceCount, line)

-- Parse a group of lines
parseLines :: Parser [(Int,String)]
parseLines = manyTill parseLine eofNewLine

-- Make an empty prepositioned tree
toPrepostionTree :: (Int,String) -> PrepositionTree String
toPrepostionTree = flip T.Node []

reverseChildren :: PrepositionTree a -> PrepositionTree a
reverseChildren (T.Node nodeLabel nodeChildren) = T.Node nodeLabel (reverse (reverseChildren <$> nodeChildren))


toTree :: PrepositionTree a -> T.Tree a
toTree prepTree = snd <$> (reverseChildren prepTree)

toParsedTree :: [PrepositionTree a] -> ParsedTree a
toParsedTree = ParsedTree . reverse . (fmap toTree)



eofNewLine :: Parser ()
eofNewLine = (eof <|> (void newline))
