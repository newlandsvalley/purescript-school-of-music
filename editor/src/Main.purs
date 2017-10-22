module Main where

import App (Event(..), foldp, initialState, view)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX)
import Audio.SoundFont (AUDIO)
import MultipleSelect.Dom (DOM)
import JS.FileIO (FILEIO)
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind, ($))
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Data.Midi.Instrument (InstrumentName(..))
import Signal (Signal, constant)
import Signal.Channel (CHANNEL)

-- initialiseApp :: forall e. Eff (ajax :: AJAX, au :: AUDIO, exception :: EXCEPTION | e) (Canceler e)
{-}
initialiseApp ::
  forall eff.
        Eff
          ( ajax :: AJAX
          , au :: AUDIO
          | eff
          )
          (Fiber
             ( ajax :: AJAX
             , au :: AUDIO
             | eff
             )
             (Array (Tuple InstrumentName (Map Int AudioBuffer)))

initialiseApp = do
  -- let's try the MJQ
  loadRemoteSoundFonts [AcousticGrandPiano, Vibraphone, AcousticBass]
          )
-}

initFonts :: Signal Event
initFonts = constant $ RequestLoadFonts [AcousticGrandPiano, Vibraphone, AcousticBass]


-- | Start and render the app
-- main :: ∀ fx. Eff (CoreEffects (fileio :: FILEIO, au :: AUDIO, vt :: VexScore.VEXTAB| fx)) Unit
-- main :: Eff (CoreEffects (ajax :: AJAX, fileio :: FILEIO, au:: AUDIO, dom :: DOM )) Unit
main :: Eff
        ( channel :: CHANNEL
        , exception :: EXCEPTION
        , ajax :: AJAX
        , au :: AUDIO
        , dom :: DOM
        , fileio :: FILEIO
        )
        Unit
main = do


  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: [ initFonts ]
    }

  renderToDOM "#app" app.markup app.input
