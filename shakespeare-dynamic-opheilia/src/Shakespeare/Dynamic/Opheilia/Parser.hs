{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Shakespeare.Dynamic.Opheilia.Parser
    ( Result (..)
    , Content (..)
    , Doc (..)
    , Nest (..)
    , Line (..)
    , TagPiece (..) 
    , parseToNestList
    , parseDoc
    , HamletSettings (..)
    , defaultHamletSettings
    , xhtmlHamletSettings
    , CloseStyle (..)
    , Binding (..)
    , NewlineStyle (..)
    , specialOrIdent
    , DataConstr (..)
    , Module (..)
    )
    where

import           Control.Applicative ((<$>), Applicative (..))
import           Control.Arrow
import           Control.Monad
import           Data.Functor.Identity
import           Data.Char (isUpper)
import           Data.Data
import           Data.Maybe (mapMaybe, fromMaybe, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Language.Haskell.TH.Syntax (Lift (..))
import           Text.Parsec.Prim (ParsecT, Stream )
import           Text.ParserCombinators.Parsec hiding (Line)
import           Text.Shakespeare.Base

data Result v = Error String | Ok v
    deriving (Show, Eq, Read, Data, Typeable)
instance Monad Result where
    return = Ok
    Error s >>= _ = Error s
    Ok v >>= f = f v
    fail = Error
instance Functor Result where
    fmap = liftM
instance Applicative Result where
    pure = return
    (<*>) = ap

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Bool Deref -- ^ bool: does it include params?
             | ContentEmbed Deref
             | ContentMsg Deref
             | ContentAttrs Deref
    deriving (Show, Eq, Read, Data, Typeable)

data Line = LineForall Deref Binding
          | LineIf Deref
          | LineElseIf Deref
          | LineElse
          | LineWith [(Deref, Binding)]
          | LineMaybe Deref Binding
          | LineNothing
          | LineCase Deref
          | LineOf Binding
          | LineTag
            { _lineTagName :: String
            , _lineAttr :: [(Maybe Deref, String, Maybe [Content])]
            , _lineContent :: [Content]
            , _lineClasses :: [(Maybe Deref, [Content])]
            , _lineAttrs :: [Deref]
            , _lineNoNewline :: Bool
            }
          | LineContent [Content] Bool -- ^ True == avoid newlines
    deriving (Eq, Show, Read)

parseLines :: HamletSettings -> String -> Result (Maybe NewlineStyle, HamletSettings, [(Int, Line)])
parseLines set s =
    case parse parser s s of
        Left e -> Error $ show e
        Right x -> Ok x
  where
    parser = do
        mnewline <- parseNewline
        let set' =
                case mnewline of
                    Nothing ->
                        case hamletNewlines set of
                            DefaultNewlineStyle -> set { hamletNewlines = AlwaysNewlines }
                            _ -> set
                    Just n -> set { hamletNewlines = n }
        res <- many (parseLine set')
        return (mnewline, set', res)

parseNewline :: ParsecT [Char] () Identity (Maybe NewlineStyle)
parseNewline =
    (try (many eol' >> spaceTabs >> string "$newline ") >> parseNewline' >>= \nl -> eol' >> return nl) <|>
    return Nothing
parseNewline' =
    (try (string "always") >> return (Just AlwaysNewlines)) <|>
    (try (string "never") >> return (Just NoNewlines)) <|>
    (try (string "text") >> return (Just NewlinesText))


parseLine :: HamletSettings -> Parser (Int, Line)
parseLine set = do
    ss <- fmap sum $ many ((char ' ' >> return 1) <|>
                           (char '\t' >> fail "Tabs are not allowed in Hamlet indentation"))
    x <- doctype set <|>
         doctypeDollar set <|>
         comment <|>
         ssiInclude <|>
         htmlComment <|>
         doctypeRaw <|>
         backslash <|>
         controlIf <|>
         controlElseIf <|>
         (try (string "$else") >> spaceTabs >> eol >> return LineElse) <|>
         controlMaybe <|>
         (try (string "$nothing") >> spaceTabs >> eol >> return LineNothing) <|>
         controlForall <|>
         controlWith <|>
         controlCase <|>
         controlOf <|>
         angle <|>
         invalidDollar <|>
         (eol' >> return (LineContent [] True)) <|>
         (do
            (cs, avoidNewLines) <- content InContent
            isEof <- (eof >> return True) <|> return False
            if null cs && ss == 0 && isEof
                then fail "End of Hamlet template"
                else return $ LineContent cs avoidNewLines)
    return (ss, x)

eol' :: ParsecT String u Identity ()
eol' = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

eol :: ParsecT String u Identity ()
eol = eof <|> eol'
doctype set = do
    try $ string "!!!" >> eol
    return $ LineContent [ContentRaw $ hamletDoctype set ++ "\n"] True
doctypeDollar set = do
    _ <- try $ string "$doctype "
    name <- many $ noneOf "\r\n"
    eol
    case lookup name $ hamletDoctypeNames set of
        Nothing -> fail $ "Unknown doctype name: " ++ name
        Just val -> return $ LineContent [ContentRaw $ val ++ "\n"] True

doctypeRaw :: ParsecT String st Identity Line
doctypeRaw = do
    x <- try $ string "<!"
    y <- many $ noneOf "\r\n"
    eol
    return $ LineContent [ContentRaw $ concat [x, y, "\n"]] True

invalidDollar :: ParsecT String u Identity b
invalidDollar = do
    _ <- char '$'
    fail "Received a command I did not understand. If you wanted a literal $, start the line with a backslash."

comment :: ParsecT String st Identity Line
comment = do
    _ <- try $ string "$#"
    _ <- many $ noneOf "\r\n"
    eol
    return $ LineContent [] True

ssiInclude :: ParsecT String st Identity Line
ssiInclude = do
    x <- try $ string "<!--#"
    y <- many $ noneOf "\r\n"
    eol
    return $ LineContent [ContentRaw $ x ++ y] False

htmlComment :: ParsecT String st Identity Line
htmlComment = do
    _ <- try $ string "<!--"
    _ <- manyTill anyChar $ try $ string "-->"
    x <- many nonComments
    eol
    return $ LineContent [ContentRaw $ concat x] False {- FIXME -} -- FIXME handle variables?

nonComments :: ParsecT String u Identity [Char]
nonComments = (many1 $ noneOf "\r\n<") <|> (do
    _ <- char '<'
    (do
        _ <- try $ string "!--"
        _ <- manyTill anyChar $ try $ string "-->"
        return "") <|> return "<")

backslash :: ParsecT [Char] u Identity Line
backslash = do
    _ <- char '\\'
    (eol >> return (LineContent [ContentRaw "\n"] True))
        <|> (uncurry LineContent <$> content InContent)

controlIf :: ParsecT String () Identity Line
controlIf = do
    _ <- try $ string "$if"
    x <- parseSpacesDeref
    eol
    return $ LineIf x

parseSpacesDeref :: ParsecT String () Identity Deref
parseSpacesDeref = do
    spaces
    x <- parseDeref
    _ <- spaceTabs
    return x


controlElseIf :: ParsecT String () Identity Line
controlElseIf = do
    _ <- try $ string "$elseif"
    x <- parseSpacesDeref
    eol
    return $ LineElseIf x

binding :: ParsecT String () Identity (Deref, Binding)
binding = do
    y <- identPattern
    spaces
    _ <- string "<-"
    spaces
    x <- parseDeref
    _ <- spaceTabs
    return (x,y)

bindingSep :: ParsecT String () Identity String
bindingSep = char ',' >> spaceTabs

controlMaybe :: ParsecT String () Identity Line
controlMaybe = do
    _ <- try $ string "$maybe"
    spaces
    (x,y) <- binding
    eol
    return $ LineMaybe x y

controlForall :: ParsecT String () Identity Line
controlForall = do
    _ <- try $ string "$forall"
    spaces
    (x,y) <- binding
    eol
    return $ LineForall x y


controlWith :: ParsecT String () Identity Line
controlWith = do
    _ <- try $ string "$with"
    spaces
    bindings <- (binding `sepBy` bindingSep) `endBy` eol
    return $ LineWith $ concat bindings -- concat because endBy returns a [[(Deref,Ident)]]


controlCase :: ParsecT String () Identity Line
controlCase = do
    _ <- try $ string "$case"
    spaces
    x <- parseDeref
    _ <- spaceTabs
    eol
    return $ LineCase x

controlOf :: ParsecT String () Identity Line
controlOf = do
    _   <- try $ string "$of"
    spaces
    x <- identPattern
    _   <- spaceTabs
    eol
    return $ LineOf x

content :: ContentRule -> ParsecT String u Identity ([Content], Bool)
content cr = do
    x <- many $ content' cr
    case cr of
        InQuotes -> void $ char '"'
        NotInQuotes -> return ()
        NotInQuotesAttr -> return ()
        InContent -> eol
    return (cc $ map fst x, any snd x)
  where
    cc [] = []
    cc (ContentRaw a:ContentRaw b:c) = cc $ ContentRaw (a ++ b) : c
    cc (a:b) = a : cc b


content' :: ContentRule -> ParsecT String u Identity (Content, Bool)
content' cr = contentHash <|> contentAt <|> contentCaret
                          <|> contentUnder
                          <|> contentReg' cr

contentHash :: ParsecT String a Identity (Content, Bool)
contentHash = do
    x <- parseHash
    case x of
        Left str -> return (ContentRaw str, null str)
        Right deref -> return (ContentVar deref, False)

contentAt :: ParsecT String a Identity (Content, Bool)
contentAt = do
    x <- parseAt
    return $ case x of
                Left str -> (ContentRaw str, null str)
                Right (s, y) -> (ContentUrl y s, False)

contentCaret :: ParsecT String a Identity (Content, Bool)
contentCaret = do
    x <- parseCaret
    case x of
        Left str -> return (ContentRaw str, null str)
        Right deref -> return (ContentEmbed deref, False)


contentUnder :: ParsecT String a Identity (Content, Bool)
contentUnder = do
    x <- parseUnder
    case x of
        Left str -> return (ContentRaw str, null str)
        Right deref -> return (ContentMsg deref, False)


contentReg'  :: Text.Parsec.Prim.Stream s m Char =>
                ContentRule -> ParsecT s u m (Content, Bool)        
contentReg' x = flip (,) False
                <$> contentReg x

contentReg
  :: Stream s m Char => ContentRule -> ParsecT s u m Content
contentReg InContent = (ContentRaw . return) <$> noneOf "#@^\r\n"
contentReg NotInQuotes = (ContentRaw . return) <$> noneOf "@^#. \t\n\r>"
contentReg NotInQuotesAttr = (ContentRaw . return) <$> noneOf "@^ \t\n\r>"
contentReg InQuotes = (ContentRaw . return) <$> noneOf "#@^\"\n\r"


tagAttribValue
  :: ContentRule -> ParsecT String u Identity [Content]
tagAttribValue notInQuotes = do
    cr <- (char '"' >> return InQuotes) <|> return notInQuotes
    fst <$> content cr

tagIdent :: ParsecT String u Identity TagPiece
tagIdent = char '#' >> TagIdent <$> tagAttribValue NotInQuotes


tagCond :: ParsecT String u Identity TagPiece
tagCond = do
    d <- between (char ':') (char ':') parseDeref
    tagClass (Just d) <|> tagAttrib (Just d)


tagClass :: Maybe Deref -> ParsecT String u Identity TagPiece
tagClass x = do
    clazz <- char '.' >> tagAttribValue NotInQuotes
    let
      hasHash (ContentRaw s) =  '#' `elem` s
      hasHash _ = False
    if any hasHash clazz
        then fail $ "Invalid class: " ++ show clazz ++ ". Did you want a space between a class and an ID?"
        else return (TagClass (x, clazz))

tagAttrib :: Maybe Deref -> ParsecT String u Identity TagPiece
tagAttrib cond = do
    s <- many1 $ noneOf " \t=\r\n><"
    v <- (char '=' >> Just <$> tagAttribValue NotInQuotesAttr) <|> return Nothing
    return $ TagAttrib (cond, s, v)

tagAttrs :: ParsecT String u Identity TagPiece
tagAttrs = do
    _ <- char '*'
    d <- between (char '{') (char '}') parseDeref
    return $ TagAttribs d

tag' :: [TagPiece]
     -> (String,
         [(Maybe Deref, String , Maybe [Content])],
         [(Maybe Deref, [Content])],
         [Deref])
tag' = foldr tag'' ("div", [], [], [])


tag'' :: TagPiece  -> (String, [(Maybe Deref, String, Maybe [Content])], [(Maybe Deref, [Content])], [Deref])
         -> (String, [(Maybe Deref, String, Maybe [Content])], [(Maybe Deref, [Content])], [Deref])
tag'' (TagName s) (_, y, z, as) = (s, y, z, as)
tag'' (TagIdent s) (x, y, z, as) = (x, (Nothing, "id", Just s) : y, z, as)
tag'' (TagClass s) (x, y, z, as) = (x, y, s : z, as)
tag'' (TagAttrib s) (x, y, z, as) = (x, s : y, z, as)
tag'' (TagAttribs s) (x, y, z, as) = (x, y, z, s : as)

ident :: Parser Ident
ident = do
  i <- many1 (alphaNum <|> char '_' <|> char '\'')
  white
  return (Ident i)
 <?> "identifier"

parens
  :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = between (char '(' >> white) (char ')' >> white)


brackets
  :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = between (char '[' >> white) (char ']' >> white)

braces
  :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = between (char '{' >> white) (char '}' >> white)

comma :: ParsecT String u Identity ()
comma = char ',' >> white


atsign :: ParsecT String u Identity ()
atsign = char '@' >> white

equals :: ParsecT String u Identity ()
equals = char '=' >> white

white :: ParsecT String u Identity ()
white = skipMany $ char ' '

wildDots :: ParsecT String u Identity ()
wildDots = string ".." >> white

isVariable (Ident (x:_)) = not (isUpper x)
isVariable (Ident []) = error "isVariable: bad identifier"

isConstructor (Ident (x:_)) = isUpper x
isConstructor (Ident []) = error "isConstructor: bad identifier"

identPattern :: Parser Binding
identPattern = gcon True <|> apat

apat :: ParsecT String () Identity Binding
apat = choice
  [ varpat
  , gcon False
  , parens tuplepat
  , brackets listpat
  ]

varpat :: ParsecT String () Identity Binding
varpat = do
  v <- try $ do v <- ident
                guard (isVariable v)
                return v
  option (BindVar v) $ do
    atsign
    b <- apat
    return (BindAs v b)
 <?> "variable"

gcon :: Bool -> Parser Binding
gcon allowArgs = do
  c <- try $ do c <- dataConstr
                return c
  choice
    [ record c
    , fmap (BindConstr c) (guard allowArgs >> many apat)
    , return (BindConstr c [])
    ]
 <?> "constructor"

dataConstr :: ParsecT String () Identity DataConstr
dataConstr = do
  p <- dcPiece
  ps <- many dcPieces
  return $ toDataConstr p ps

dcPieces :: ParsecT String () Identity String
dcPiece = do
  x@(Ident y) <- ident
  guard $ isConstructor x
  return y

dcPieces = do
  _ <- char '.'
  dcPiece

toDataConstr x [] = DCUnqualified $ Ident x
toDataConstr x (y:ys) =
    go (x:) y ys
  where
    go front next [] = DCQualified (Module $ front []) (Ident next)
    go front next (rest:rests) = go (front . (next:)) rest rests

record :: DataConstr -> ParsecT String () Identity Binding
record c = braces $ do
  (fields, wild) <- option ([], False) $ go
  return (BindRecord c fields wild)
  where
  go = (wildDots >> return ([], True))
     <|> (do x         <- recordField
             (xs,wild) <- option ([],False) (comma >> go)
             return (x:xs,wild))

recordField :: ParsecT String () Identity (Ident, Binding)
recordField = do
  field <- ident
  p <- option (BindVar field) -- support punning
              (equals >> identPattern)
  return (field,p)

tuplepat = do
  xs <- identPattern `sepBy` comma
  return $ case xs of
    [x] -> x
    _   -> BindTuple xs

listpat :: ParsecT String () Identity Binding
listpat = BindList <$> identPattern `sepBy` comma

angle :: ParsecT String u Identity Line
angle = do
    _ <- char '<'
    name' <- many  $ noneOf " \t.#\r\n!>"
    let name = if null name' then "div" else name'
    xs <- many $ try ((many $ oneOf " \t\r\n") >>
          (tagIdent <|> tagCond <|> tagClass Nothing <|> tagAttrs <|> tagAttrib Nothing))
    _ <- many $ oneOf " \t\r\n"
    _ <- char '>'
    (c, avoidNewLines) <- content InContent
    let (tn, attr, classes, attrsd) = tag' $ TagName name : xs
    if '/' `elem` tn
      then fail "A tag name may not contain a slash. Perhaps you have a closing tag in your HTML."
      else return $ LineTag tn attr c classes attrsd avoidNewLines

data TagPiece = TagName String
              | TagIdent [Content]
              | TagClass (Maybe Deref, [Content])
              | TagAttrib (Maybe Deref, String, Maybe [Content])
              | TagAttribs Deref
    deriving Show

data ContentRule = InQuotes | NotInQuotes | NotInQuotesAttr | InContent

data Nest = Nest Line [Nest]

nestLines :: [(Int, Line)] -> [Nest]
nestLines [] = []
nestLines ((i, l):rest) =
    let (deeper, rest') = span (\(i', _) -> i' > i) rest
     in Nest l (nestLines deeper) : nestLines rest'

data Doc = DocForall Deref Binding [Doc]
         | DocWith [(Deref, Binding)] [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocMaybe Deref Binding [Doc] (Maybe [Doc])
         | DocCase Deref [(Binding, [Doc])]
         | DocContent Content
    deriving (Show, Eq, Read, Data, Typeable)

nestToDoc :: HamletSettings -> [Nest] -> Result [Doc]
nestToDoc _set [] = Ok []
nestToDoc set (Nest (LineForall d i) inside:rest) = do
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ DocForall d i inside' : rest'
nestToDoc set (Nest (LineWith dis) inside:rest) = do
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ DocWith dis inside' : rest'
nestToDoc set (Nest (LineIf d) inside:rest) = do
    inside' <- nestToDoc set inside
    (ifs, el, rest') <- parseConds set ((:) (d, inside')) rest
    rest'' <- nestToDoc set rest'
    Ok $ DocCond ifs el : rest''
nestToDoc set (Nest (LineMaybe d i) inside:rest) = do
    inside' <- nestToDoc set inside
    (nothing, rest') <-
        case rest of
            Nest LineNothing ninside:x -> do
                ninside' <- nestToDoc set ninside
                return (Just ninside', x)
            _ -> return (Nothing, rest)
    rest'' <- nestToDoc set rest'
    Ok $ DocMaybe d i inside' nothing : rest''
nestToDoc set (Nest (LineCase d) inside:rest) = do
    let getOf (Nest (LineOf x) insideC) = do
            insideC' <- nestToDoc set insideC
            Ok (x, insideC')
        getOf _ = Error "Inside a $case there may only be $of.  Use '$of _' for a wildcard."
    cases <- mapM getOf inside
    rest' <- nestToDoc set rest
    Ok $ DocCase d cases : rest'
nestToDoc set (Nest (LineTag tn attrs contentInLineTag classes attrsD avoidNewLine) inside:rest) = do
    let attrFix (x, y, z) = (x, y, [(Nothing, z)])
    let takeClass (a, "class", b) = Just (a, fromMaybe [] b)
        takeClass _ = Nothing
    let clazzes = classes ++ mapMaybe takeClass attrs
    let notClass (_, x, _) = x /= "class"
    let noclass = filter notClass attrs
    let attrs' =
            case clazzes of
              [] -> map attrFix noclass
              _ -> (testIncludeClazzes clazzes, "class", map (second Just) clazzes)
                       : map attrFix noclass
    let closeStyle =
            if not (null contentInLineTag) || not (null inside)
                then CloseSeparate
                else hamletCloseStyle set tn
    let end = case closeStyle of
                CloseSeparate ->
                    DocContent $ ContentRaw $ "</" ++ tn ++ ">"
                _ -> DocContent $ ContentRaw ""
        seal = case closeStyle of
                 CloseInside -> DocContent $ ContentRaw "/>"
                 _ -> DocContent $ ContentRaw ">"
        start = DocContent $ ContentRaw $ "<" ++ tn
        attrs'' = concatMap attrToContent attrs'
        newline' = DocContent $ ContentRaw
                 $ case hamletNewlines set of { AlwaysNewlines | not avoidNewLine -> "\n"; _ -> "" }
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ start
       : attrs''
      ++ map (DocContent . ContentAttrs) attrsD
      ++ seal
       : map DocContent contentInLineTag
      ++ inside'
      ++ end
       : newline'
       : rest'
nestToDoc set (Nest (LineContent contentInLineTag avoidNewLine) inside:rest) = do
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    let newline' = DocContent $ ContentRaw
                   $ case hamletNewlines set of { NoNewlines -> ""; _ -> if nextIsContent && not avoidNewLine then "\n" else "" }
        nextIsContent =
            case (inside, rest) of
                ([], Nest LineContent{} _:_) -> True
                ([], Nest LineTag{} _:_) -> True
                _ -> False
    Ok $ map DocContent contentInLineTag ++ newline':inside' ++ rest'
nestToDoc _set (Nest (LineElseIf _) _:_) = Error "Unexpected elseif"
nestToDoc _set (Nest LineElse _:_) = Error "Unexpected else"
nestToDoc _set (Nest LineNothing _:_) = Error "Unexpected nothing"
nestToDoc _set (Nest (LineOf _) _:_) = Error "Unexpected 'of' (did you forget a $case?)"

compressDoc :: [Doc] -> [Doc]
compressDoc [] = []
compressDoc (DocForall d i doc:rest) =
    DocForall d i (compressDoc doc) : compressDoc rest
compressDoc (DocWith dis doc:rest) =
    DocWith dis (compressDoc doc) : compressDoc rest
compressDoc (DocMaybe d i doc mnothing:rest) =
    DocMaybe d i (compressDoc doc) (fmap compressDoc mnothing)
  : compressDoc rest
compressDoc (DocCond [(a, x)] Nothing:DocCond [(b, y)] Nothing:rest)
    | a == b = compressDoc $ DocCond [(a, x ++ y)] Nothing : rest
compressDoc (DocCond x y:rest) =
    DocCond (map (second compressDoc) x) (compressDoc `fmap` y)
    : compressDoc rest
compressDoc (DocCase d cs:rest) =
    DocCase d (map (second compressDoc) cs) : compressDoc rest
compressDoc (DocContent (ContentRaw ""):rest) = compressDoc rest
compressDoc ( DocContent (ContentRaw x)
            : DocContent (ContentRaw y)
            : rest
            ) = compressDoc $ (DocContent $ ContentRaw $ x ++ y) : rest
compressDoc (DocContent x:rest) = DocContent x : compressDoc rest

parseDoc :: HamletSettings -> String -> Result (Maybe NewlineStyle, [Doc])
parseDoc set s = do
    (mnl, set', ns) <- parseToNestList set s 
    ds <- nestToDoc set' ns
    return (mnl, compressDoc ds)

parseToNestList :: HamletSettings -> String -> Result (Maybe NewlineStyle, HamletSettings, [Nest])
parseToNestList set s = do
  (mnl, set', ls) <- parseLines set s
  let notEmpty (_, LineContent [] _) = False
      notEmpty _ = True
  let ns = nestLines $ filter notEmpty ls
  return (mnl,set',ns)

attrToContent :: (Maybe Deref, String, [(Maybe Deref, Maybe [Content])]) -> [Doc]
attrToContent (Just cond, k, v) =
    [DocCond [(cond, attrToContent (Nothing, k, v))] Nothing]
attrToContent (Nothing, k, []) = [DocContent $ ContentRaw $ ' ' : k]
attrToContent (Nothing, k, [(Nothing, Nothing)]) = [DocContent $ ContentRaw $ ' ' : k]
attrToContent (Nothing, k, [(Nothing, Just v)]) =
    DocContent (ContentRaw (' ' : k ++ "=\""))
  : map DocContent v
  ++ [DocContent $ ContentRaw "\""]
attrToContent (Nothing, k, v) = -- only for class
      DocContent (ContentRaw (' ' : k ++ "=\""))
    : concatMap go (init v)
    ++ go' (last v)
    ++ [DocContent $ ContentRaw "\""]
  where
    go (Nothing, x) = map DocContent (fromMaybe [] x) ++ [DocContent $ ContentRaw " "]
    go (Just b, x) =
        [ DocCond
            [(b, map DocContent (fromMaybe [] x) ++ [DocContent $ ContentRaw " "])]
            Nothing
        ]
    go' (Nothing, x) = maybe [] (map DocContent) x
    go' (Just b, x) =
        [ DocCond
            [(b, maybe [] (map DocContent) x)]
            Nothing
        ]

-- | Settings for parsing of a hamlet document.
data HamletSettings = HamletSettings
    {
      -- | The value to replace a \"!!!\" with. Do not include the trailing
      -- newline.
      hamletDoctype :: String
      -- | Should we add newlines to the output, making it more human-readable?
      --  Useful for client-side debugging but may alter browser page layout.
    , hamletNewlines :: NewlineStyle
      -- | How a tag should be closed. Use this to switch between HTML, XHTML
      -- or even XML output.
    , hamletCloseStyle :: String -> CloseStyle
      -- | Mapping from short names in \"$doctype\" statements to full doctype.
    , hamletDoctypeNames :: [(String, String)]
    }

data NewlineStyle = NoNewlines -- ^ never add newlines
                  | NewlinesText -- ^ add newlines between consecutive text lines
                  | AlwaysNewlines -- ^ add newlines everywhere
                  | DefaultNewlineStyle
    deriving Show

instance Lift NewlineStyle where
    lift NoNewlines = [|NoNewlines|]
    lift NewlinesText = [|NewlinesText|]
    lift AlwaysNewlines = [|AlwaysNewlines|]
    lift DefaultNewlineStyle = [|DefaultNewlineStyle|]

instance Lift (String -> CloseStyle) where
    lift _ = [|\s -> htmlCloseStyle s|]

instance Lift HamletSettings where
    lift (HamletSettings a b c d) = [|HamletSettings $(lift a) $(lift b) $(lift c) $(lift d)|]


htmlEmptyTags :: Set String
htmlEmptyTags = Set.fromAscList
    [ "area"
    , "base"
    , "basefont"
    , "br"
    , "col"
    , "frame"
    , "hr"
    , "img"
    , "input"
    , "isindex"
    , "link"
    , "meta"
    , "param"
    ]

-- | Defaults settings: HTML5 doctype and HTML-style empty tags.
defaultHamletSettings :: HamletSettings
defaultHamletSettings = HamletSettings "<!DOCTYPE html>" DefaultNewlineStyle htmlCloseStyle doctypeNames

xhtmlHamletSettings :: HamletSettings
xhtmlHamletSettings =
    HamletSettings xhtmlDoctype DefaultNewlineStyle xhtmlCloseStyle doctypeNames
  where
    xhtmlDoctype =
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ++
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

htmlCloseStyle :: String -> CloseStyle
htmlCloseStyle s =
    if Set.member s htmlEmptyTags
        then NoClose
        else CloseSeparate

xhtmlCloseStyle :: String -> CloseStyle
xhtmlCloseStyle s =
    if Set.member s htmlEmptyTags
        then CloseInside
        else CloseSeparate

data CloseStyle = NoClose | CloseInside | CloseSeparate

parseConds :: HamletSettings
           -> ([(Deref, [Doc])] -> [(Deref, [Doc])])
           -> [Nest]
           -> Result ([(Deref, [Doc])], Maybe [Doc], [Nest])
parseConds set front (Nest LineElse inside:rest) = do
    inside' <- nestToDoc set inside
    Ok (front [], Just inside', rest)
parseConds set front (Nest (LineElseIf d) inside:rest) = do
    inside' <- nestToDoc set inside
    parseConds set (front . (:) (d, inside')) rest
parseConds _ front rest = Ok (front [], Nothing, rest)

doctypeNames :: [(String, String)]
doctypeNames =
    [ ("5", "<!DOCTYPE html>")
    , ("html", "<!DOCTYPE html>")
    , ("1.1", "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
    , ("strict", "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
    ]

data Binding = BindVar Ident
             | BindAs Ident Binding
             | BindConstr DataConstr [Binding]
             | BindTuple [Binding]
             | BindList [Binding]
             | BindRecord DataConstr [(Ident, Binding)] Bool
    deriving (Eq, Show, Read, Data, Typeable)

data DataConstr = DCQualified Module Ident
                | DCUnqualified Ident
    deriving (Eq, Show, Read, Data, Typeable)

newtype Module = Module [String]
    deriving (Eq, Show, Read, Data, Typeable)

spaceTabs :: Parser String
spaceTabs = many $ oneOf " \t"

-- | When using conditional classes, it will often be a single class, e.g.:
--
-- > <div :isHome:.homepage>
--
-- If isHome is False, we do not want any class attribute to be present.
-- However, due to combining multiple classes together, the most obvious
-- implementation would produce a class="". The purpose of this function is to
-- work around that. It does so by checking if all the classes on this tag are
-- optional. If so, it will only include the class attribute if at least one
-- conditional is true.
testIncludeClazzes :: [(Maybe Deref, [Content])] -> Maybe Deref
testIncludeClazzes cs
    | any (isNothing . fst) cs = Nothing
    | otherwise = Just $ DerefBranch (DerefIdent specialOrIdent) $ DerefList $ mapMaybe fst cs

-- | This funny hack is to allow us to refer to the 'or' function without
-- requiring the user to have it in scope. See how this function is used in
-- Text.Hamlet.

specialOrIdent :: Ident
specialOrIdent = Ident "__or__hamlet__special"


