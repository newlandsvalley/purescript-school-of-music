module FileIO.FileIO
  ( FILEIO
  , Filespec
  , loadTextFile
  , saveTextFile ) where

import Prelude (Unit)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Aff (Aff, makeAff)


type Filespec =
  {
    contents :: String
  , name :: String
  }

-- | File IO Effect
foreign import data FILEIO :: Effect

foreign import loadTextFileImpl :: forall e. String -> (Filespec -> Eff e Unit) -> Eff e Unit

loadTextFile :: forall e. String -> Aff e Filespec
loadTextFile id = makeAff (\error success -> (loadTextFileImpl id) success)

foreign import saveTextFile :: forall eff. Filespec -> Eff (fileio :: FILEIO | eff) Unit
