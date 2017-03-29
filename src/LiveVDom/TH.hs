{-# LANGUAGE TemplateHaskell #-}
module LiveVDom.TH where

import qualified Data.JSString              as JS (unpack)

import           LiveVDom.Types
-- Template haskell related
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import qualified Data.Sequence                 as S
import           LiveVDom.Internal

-- | Use template haskell to create the live vdom
-- the resulting type is Desc ()
toLiveVDomTH :: PLiveVDom -> Q Exp
toLiveVDomTH (PLiveVText st) = do
  iStr <- lift $ JS.unpack st
  return $ AppE (VarE 'buildStaticText) iStr
toLiveVDomTH (PLiveVNode tn ns pl ch) = do
  qtn <- lift tn
  qns <- lift $ fmap JS.unpack ns
  qpl <- lift pl
  cExp <- sequence $ toLiveVDomTH <$> ch
  return $ AppE (AppE (AppE (AppE (VarE 'buildLiveVNode) qtn) qns) qpl) (AppE (VarE 'S.fromList) (ListE cExp))
toLiveVDomTH (PLiveChildren e) = return $ AppE (VarE  'buildLiveChildren) (AppE (VarE 'toDesc) e)
toLiveVDomTH (PLiveInterpText t) = return $ AppE (VarE 'buildLiveVText) t
