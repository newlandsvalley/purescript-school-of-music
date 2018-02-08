module MultipleSelect.Dom
  ( SDOM
  , resetDefaultSelected ) where

import Prelude (Unit)
import Control.Monad.Eff (kind Effect, Eff)

-- | DOM Effect
-- | we;ll call it SDOM to distinguish from 'proper' DOM effects in Halogen
foreign import data SDOM :: Effect

-- | We use FFI into javascript just in order to reset the selected option
-- | in the select menu to the default value whenever something else
-- | has been chosen
foreign import resetDefaultSelected :: forall eff. Eff (sdom :: SDOM | eff) Unit
