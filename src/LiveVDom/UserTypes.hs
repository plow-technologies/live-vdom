module LiveVDom.UserTypes (LiveVDom) where

import           LiveVDom.Adapter.Types
import qualified LiveVDom.Types as Types

-- | LiveVDom elements, generated by valentine and consumed by 'runDom'
type LiveVDom = Types.LiveVDom JSEvent