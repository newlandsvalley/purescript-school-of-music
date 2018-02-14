module Main where

import Prelude
import Audio.SoundFont (AUDIO, Instrument, loadRemoteSoundFonts)
import Data.Midi.Instrument (InstrumentName(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H

import JS.FileIO (FILEIO)
import MultipleSelect.Dom (SDOM)
import Network.HTTP.Affjax (AJAX)

import Container as Container

loadInstruments :: ∀ eff .
  Aff ( ajax :: AJAX, au ::AUDIO | eff)
    (Array Instrument)
loadInstruments =
  loadRemoteSoundFonts  [AcousticGrandPiano, Vibraphone, AcousticBass]

main :: Eff (HA.HalogenEffects (ajax :: AJAX, au :: AUDIO, fileio :: FILEIO, sdom :: SDOM )) Unit
main = HA.runHalogenAff do
  instruments <- H.liftAff loadInstruments
  body <- HA.awaitBody
  runUI (Container.component instruments) unit body
