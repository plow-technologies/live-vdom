module Shakespeare.Dynamic where

import Shakespeare.Dynamic.Internal


import Text.Hamlet.Parse

import GHCJS.VDOM

{-| Hamlet.Parse Data Types Imported for reference

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


data Doc = DocForall Deref Binding [Doc]
         | DocWith [(Deref, Binding)] [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocMaybe Deref Binding [Doc] (Maybe [Doc])
         | DocCase Deref [(Binding, [Doc])]
         | DocContent Content
    deriving (Show, Eq, Read, Data, Typeable)

data NewlineStyle = NoNewlines -- ^ never add newlines
                  | NewlinesText -- ^ add newlines between consecutive text lines
                  | AlwaysNewlines -- ^ add newlines everywhere
                  | DefaultNewlineStyle
    deriving Show


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

|-}



testParseLines = parseDoc defaultHamletSettings "<h1> test" 
