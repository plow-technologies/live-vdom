{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia where


import           BasicPrelude
import           Control.Applicative
import           Data.Foldable
import           Data.String.Here


import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta.Delta     as D
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

data Lines = Lines {
  lineLine     :: String
, currentLevel :: Int
, lineChildren :: [Lines]
} deriving (Eq, Show)


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


toLines :: (Int,String) -> Lines
toLines (level,val) = Lines val level []

reverseChildren :: Lines -> Lines
reverseChildren (Lines l lv ch) = Lines l lv $ reverse (reverseChildren <$> ch) 

parseLineForrest :: Parser [Lines]
parseLineForrest = do
  xs <- parseLines
  xs' <- foldlM insertAtLevel [] (toLines <$> xs)
  return . reverse $ reverseChildren <$> xs'

insertAtLevel :: (Monad m, Functor m) => [Lines] -> Lines -> m [Lines]
insertAtLevel [] new = return $ [new]
insertAtLevel (old:xs) new = case compare newLevel oldLevel of
                              EQ -> return $ new:old:xs
                              GT -> (flip (:) xs) <$> inserted
                              LT -> return $ new:old:xs-- fail $ "Unable to add " ++ (lineLine new) ++ ". Unable to find root"  -- Case should never happen
  where newLevel = currentLevel new
        oldLevel = currentLevel old
        inserted = addInsert old new
        addInsert (Lines val level ch) newI = Lines val level <$> insertAtLevel ch newI

eofNewLine :: Parser ()
eofNewLine = (eof <|> (void newline))

sampleChildren :: String
sampleChildren = [here|
Parent One
  Child of Parent One 1
    Child of Child of Parent One
   Child of Parent One 2
Parent Two
  Child of Parent Two

|]

-- [Lines "Parent One" 0 [
--     Lines "Child of Parent One 1" 2
--       [Lines "Child of Child of Parent One" 4 []
--        Lines "Child of Parent One 2" 2 []]
--   Lines "Parent Two" 0
--     [Lines "Child of Parent Two" 2 []]]]


sampleLines :: [Lines]
sampleLines = [
                Lines "Parent One" 0 [Lines "Child of Parent One 1" 2 [Lines "Child of Child of Parent One" 4 [], Lines "Child of Parent One 2" 2 []]]
              , Lines "Parent Two" 0 [Lines "Child of Parent Two" 2 []]
              ]


testLines :: IO Bool
testLines = do
  let Success lList = parseString parseLineForrest (D.Columns 0 0) sampleChildren
  print lList
  print sampleLines
  return (lList == sampleLines)
