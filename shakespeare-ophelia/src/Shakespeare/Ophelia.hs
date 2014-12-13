{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shakespeare.Ophelia where


import           Control.Applicative
import           Data.Functor
import           Data.String.Here
import           Data.Text
import           Data.Tree
import           Debug.Trace
import           Prelude
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta.Parser

data Lines = Lines {
  line     :: String
, children :: [Lines]
} deriving (Eq, Show)



splitLines :: Parser [[Char]]
splitLines = manyTill (manyTill anyChar newline) eof

spacesCount :: Parser Int
spacesCount = Prelude.length <$> many (char ' ')


pLine :: Parser (Int,String)
pLine = do
  whiteSpaceCount <- spacesCount
  line <- manyTill anyChar eofNewLine
  return (whiteSpaceCount, line)

pLineList :: Int -> Parser (Int, String)
pLineList level = undefined


-- prependParentLevels :: [(Int, String)] -> [([Int],String)]
-- prependParentLevels x = fst $ foldl foldFunc [] x
--   where foldFunc :: [([Int],(Int, String))] -> (([Int],String))
-- lineTree :: [(Int, String)] -> [Lines]
-- lineTree xs = lineList
--   where lineList = foldl foldFunc [] xs

-- foldFunc lineList node =
--   mLast = lastMay lineList






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
