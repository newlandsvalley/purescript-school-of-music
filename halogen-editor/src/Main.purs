module Main where

import Prelude
import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Data.Midi.Instrument (InstrumentName(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H

import Container as Container

loadInstruments ::  Aff (Array Instrument)
loadInstruments =
  loadRemoteSoundFonts  [AcousticGrandPiano, Vibraphone, AcousticBass]

main :: Effect Unit
main = HA.runHalogenAff do
  instruments <- H.liftAff loadInstruments
  body <- HA.awaitBody
  runUI (Container.component instruments) unit body
