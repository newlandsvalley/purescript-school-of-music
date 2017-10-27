module Main where

import App (Event(..), foldp, initialState, view)
import Network.HTTP.Affjax (AJAX)
import Audio.SoundFont (AUDIO)
import JS.FileIO (FILEIO)
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind, ($))
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Data.Midi.Instrument (InstrumentName(..))
import Signal (Signal, constant)

initFonts :: Signal Event
initFonts = constant $ RequestLoadFonts [AcousticGrandPiano, Vibraphone, AcousticBass]

-- | Start and render the app
-- main :: ∀ fx. Eff (CoreEffects (fileio :: FILEIO, au :: AUDIO, vt :: VexScore.VEXTAB| fx)) Unit
main :: Eff (CoreEffects (ajax :: AJAX, fileio :: FILEIO, au:: AUDIO )) Unit
main = do

  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: [ initFonts ]
    }

  renderToDOM "#app" app.markup app.input
