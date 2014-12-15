{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia.Parser (
  parseLineForest
, ParsedTree(..)
, fromTree
)
where


import           BasicPrelude            hiding (foldl)
import           Control.Applicative
import           Data.Foldable
import qualified Data.Tree               as T


import           Data.Traversable
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta.Delta
import           Text.Trifecta.Parser
import           Text.Trifecta.Result


type PrepositionTree a = T.Tree (Int,a)
newtype ParsedTree a = ParsedTree { unParsedTree :: [T.Tree a] } deriving (Eq, Show)


fromTree :: Parser ([a] -> a) -> ParsedTree String -> Result [a]
fromTree builder (ParsedTree xs) = traverse fromTree' xs
  where fromTree' (T.Node st ch) = res <*> children
          where res = parseString builder (Columns 0 0) st
                children = traverse fromTree' ch

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

parseLineT :: Parser (PrepositionTree String)
parseLineT = do
  label <- parseLine
  return $ T.Node label []

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
