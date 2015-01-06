module Shakespeare.Ophelia.QQ where

import           Control.Monad.IO.Class
import           Data.List.Split
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Shakespeare.Ophelia.Parser
import           Shakespeare.Ophelia.Parser.VDOM
import           Text.PrettyPrint.ANSI.Leijen
import           Text.Trifecta.Result
import           VDOM.Adapter

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


gertrude :: QuasiQuoter
gertrude = QuasiQuoter gertrudeExp undefined undefined undefined

buildF :: String -> Q Exp
buildF str = return $ foldl (\e app -> AppE e (VarE $ mkName app)) (VarE . mkName . head $ xs) $ tail xs
  where xs = splitOn " " str

bqq :: QuasiQuoter
bqq = QuasiQuoter buildF undefined undefined undefined
