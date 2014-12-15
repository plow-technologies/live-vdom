{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia where


import           BasicPrelude
import           Control.Applicative
import           Data.Foldable
import qualified Data.Tree as T


import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta.Parser


type PrepositionTree a = T.Tree (Int,a)
newtype ParsedTree a = ParsedTree { unParsedTree :: [T.Tree a] } deriving (Eq, Show)


splitLines :: Parser [[Char]]
splitLines = manyTill (manyTill anyChar newline) eof

spacesCount :: Parser Int
spacesCount = length <$> many (char ' ')


parseLine :: Parser (Int,String)
parseLine = do
  whiteSpaceCount <- spacesCount
  line <- manyTill anyChar eofNewLine
  return (whiteSpaceCount, line)


parseLines :: Parser [(Int,String)]
parseLines = manyTill parseLine eofNewLine


toPrepostionTree :: (Int,String) -> PrepositionTree String
toPrepostionTree = flip T.Node []

reverseChildren :: PrepositionTree a -> PrepositionTree a
reverseChildren (T.Node nodeLabel nodeChildren) = T.Node nodeLabel (reverse (reverseChildren <$> nodeChildren))


toTree :: PrepositionTree a -> T.Tree a
toTree prepTree = snd <$> (reverseChildren prepTree)

toParsedTree :: [PrepositionTree a] -> ParsedTree a
toParsedTree = ParsedTree . reverse . (fmap toTree)

parseLineForest :: Parser (ParsedTree String)
parseLineForest = do
  xs <- parseLines
  xs' <- foldlM insertAtLevel [] (toPrepostionTree <$> xs)
  return $ toParsedTree xs'

insertAtLevel :: (Monad m, Functor m) => [PrepositionTree a] -> PrepositionTree a -> m [PrepositionTree a]
insertAtLevel [] new = return $ [new]
insertAtLevel (old:xs) new = case compare newLevel oldLevel of
                              EQ -> return $ new:old:xs
                              GT -> (flip (:) xs) <$> inserted
                              LT -> return $ new:old:xs-- fail $ "Unable to add " ++ (lineLine new) ++ ". Unable to find root"  -- Case should never happen
  where newLevel = currentLevel new
        oldLevel = currentLevel old
        currentLevel = fst . T.rootLabel
        inserted = addInsert old new
        addInsert (T.Node label ch) newI = T.Node label <$> insertAtLevel ch newI

eofNewLine :: Parser ()
eofNewLine = (eof <|> (void newline))