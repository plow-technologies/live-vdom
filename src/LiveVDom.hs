module LiveVDom (
    module LiveVDom.Components
  , module LiveVDom.Event
  , module LiveVDom.Message
  , module LiveVDom.Render
  , module Control.Concurrent.STM.Notify
  , LiveVDom
  ) where

import           Control.Concurrent.STM.Notify
import           LiveVDom.Adapter.Types
import           LiveVDom.Components
import           LiveVDom.Event
import           LiveVDom.Message
import           LiveVDom.Render
import qualified LiveVDom.Types as Types

-- | Return type of quasi-quoter and type expected by runDom
type LiveVDom = Types.LiveVDom JSEvent
