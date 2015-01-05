
{-# LANGUAGE QuasiQuotes #-}
module Shakespeare.Ophelia where
import           VDOM.Adapter

import Shakespeare.Ophelia.QQ

test :: [VNodeAdapter]
test = [ophelia|<table style="width:100%">
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
