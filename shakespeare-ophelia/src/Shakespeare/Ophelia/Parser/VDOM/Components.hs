module Shakespeare.Ophelia.Parser.VDOM.Components where

import           Control.Concurrent.STM.Notify


import           Shakespeare.Ophelia.Parser.VDOM.Event
import           Shakespeare.Ophelia.Parser.VDOM.Types

button :: Address (Event ()) -> LiveVDom
button _addr = undefined

