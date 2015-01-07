
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Shakespeare.Ophelia where
import           VDOM.Adapter

import           Data.List.Split
import           Language.Haskell.TH
import           Shakespeare.Ophelia.QQ
import           Language.Haskell.TH.Quote
import Pipes.Concurrent

test1 = [gertrude|<table style="width:100%">
                  <tr>
                    <td>
                      Jill
                    <td>
                      Smith
                    <td>
                      50
  <tr>
    <td>
      Evan
    <td>
      Jackson
    <td>
      941

|]


test3 = [gertrude|<table style="width:100%">
                  <tr>
                    <td>
                      Jill
                    <td>
                      Smith
                    <td>
                      50
|]

pc = spawn $ Latest test1

-- woah :: String
-- woah = $(stringL "print . show $ 4")

test2 o =  do
  [gertrude|<table style="width:100%">
                  <tr>
                    <td>
                      Jill
                    <td>
                      Smith
                    <td>
                      50
                    !{o}
                  <tr>
                    <td>
                      Evan
                      ${rslt2}
                    <td>
                      Jackson
                    <td>
                      941
|]

test4 a = [gertrude|
<hello some="test">
  #{show a}
|]


