
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Shakespeare.Ophelia where
import           VDOM.Adapter

import           Data.List.Split
import           Language.Haskell.TH
import           Shakespeare.Ophelia.QQ
import           Language.Haskell.TH.Quote

test :: VNodeAdapter
test = [gertrude|<table style="width:100%">
                  <tr>
                    <td>
                      Jill
                    <td>
                      Smith
                    <td>
                      50
  <tr>
    <td>\
      Evan
    <td>
      Jackson
    <td>
      941

|]


test2 :: VNodeAdapter
test2 =  do
  [gertrude|<table style="width:100%">
                  <tr>
                    <td>
                      Jill
                    <td>
                      Smith
                    <td>
                      50
                    ${rslt}
                  <tr>
                    <td>
                      Evan
                      ${rslt2}
                    <td>
                      Jackson
                    <td>
                      941

|]

xs t = [bqq|t|]

