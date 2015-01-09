module Shakespeare.Ophelia.QQ (
  ophelia
, gertrude
, module Shakespeare.Ophelia.Parser.VDOM.Types
, module Shakespeare.Ophelia.Parser.VDOM.Live
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax


import           Shakespeare.Ophelia.Parser
import           Shakespeare.Ophelia.Parser.VDOM

import           Shakespeare.Ophelia.Parser.VDOM.Types
import           Shakespeare.Ophelia.Parser.VDOM.Live


import           Text.Trifecta.Result

opheliaExp :: String -> Q Exp
opheliaExp s = do
  rN <- parseVNodeS s
  case rN of
    Success vn -> lift vn
    Failure fString -> fail $ show fString

ophelia :: QuasiQuoter
ophelia = QuasiQuoter opheliaExp undefined undefined undefined

liveGertrude :: String -> Q Exp
liveGertrude s = do
  rN <- parseStringTrees parsePLiveVDom s
  case rN of
    Success vn -> if length vn > 1
                    then fail "One or more nodes can not be the main html. Maybe you're trying to use ophelia?"
                    else toLiveVDomTH $ vn !! 0
    Failure fString -> fail $ show fString

gertrude :: QuasiQuoter
gertrude = QuasiQuoter liveGertrude undefined undefined undefined
