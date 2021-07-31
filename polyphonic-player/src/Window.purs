module Window (print) where

import Prelude (Unit)
import Effect (Effect)

foreign import print
  :: Effect Unit
