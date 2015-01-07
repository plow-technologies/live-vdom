module Shakespeare.Ophelia.QQ (
  ophelia
, gertrude
, module Shakespeare.Ophelia.Parser.Live.Types
, module Shakespeare.Ophelia.Parser.Live.VDOM 
) where

import           Control.Monad.IO.Class
import           Data.List.Split



import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax


import           Shakespeare.Ophelia.Parser
import           Shakespeare.Ophelia.Parser.VDOM
import           VDOM.Adapter

import Shakespeare.Ophelia.Parser.Live.Types
import Shakespeare.Ophelia.Parser.Live.VDOM


import           Text.PrettyPrint.ANSI.Leijen
import           Text.Trifecta.Result

opheliaExp :: String -> Q Exp
opheliaExp s = do
  rN <- parseVNodeS s
  case rN of
    Success vn -> lift vn
    Failure fString -> fail $ show fString

ophelia :: QuasiQuoter
ophelia = QuasiQuoter opheliaExp undefined undefined undefined



gertrudeExp :: String -> Q Exp
gertrudeExp s = do
  rN <- parseVNodeS s
  case rN of
    Success vn -> if length vn > 1
                    then fail "One or more nodes can not be the main html. Maybe you're trying to use ophelia?"
                    else lift (vn !! 0)
    Failure fString -> fail $ show fString


liveGertrude :: String -> Q Exp
liveGertrude s = do
  rN <- parseStringTrees parsePLiveVDom s
  case rN of
    Success vn -> if length vn > 1
                    then fail "One or more nodes can not be the main html. Maybe you're trying to use ophelia?"
                    else toLiveVDomTH (vn !! 0)
    Failure fString -> fail $ show fString




gertrude :: QuasiQuoter
gertrude = QuasiQuoter liveGertrude undefined undefined undefined
