let conf = ./spago.dhall

in conf // {
  sources = {- conf.sources # -} [ "polyphonic-player/**/*.purs" ],
  dependencies = conf.dependencies # [ "abc-parser"
                                     , "abc-scores"
                                     , "abc2psom"
                                     , "aff"
                                     , "console"
                                     , "dom-indexed"
                                     , "effect"
                                     , "ensemble-scores"
                                     , "halogen"
                                     , "halogen-components"
                                     , "media-types"
                                     , "partial"
                                     , "school-of-music"
                                     , "soundfonts" ],
  packages = ./polyphonic-player-packages.dhall
}
