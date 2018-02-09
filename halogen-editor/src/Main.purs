module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import JS.FileIO (FILEIO)
import MultipleSelect.Dom (SDOM)
import Audio.SoundFont (AUDIO)
import Network.HTTP.Affjax (AJAX)

import Container as Container

main :: Eff (HA.HalogenEffects (ajax :: AJAX, au :: AUDIO, fileio :: FILEIO, sdom :: SDOM )) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Container.component unit body
