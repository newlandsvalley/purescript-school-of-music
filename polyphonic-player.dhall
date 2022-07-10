let conf = ./spago.dhall

in conf // {
  sources = {- conf.sources # -} [ "polyphonic-player/**/*.purs" ],
  dependencies = conf.dependencies # [ "abc-parser"
                                     , "abc-scores"
                                     , "abc2psom"
                                     , "aff"
                                     , "colors"
                                     , "console"
                                     , "css"
                                     , "debug"
                                     , "dom-indexed"
                                     , "effect"
                                     , "ensemble-scores"
                                     , "halogen"
                                     , "halogen-css"
                                     , "halogen-components"
                                     , "media-types"
                                     , "partial"
                                     , "school-of-music"
                                     , "soundfonts" ],
  packages = ./polyphonic-player-packages.dhall
}
