let conf = ./spago.dhall

in conf // {
  sources = {- conf.sources # -} [ "polyphonic-player/**/*.purs" ],
  dependencies = conf.dependencies # [ "abc-parser"
                                     , "abc-scores"
                                     , "abc2psom"
                                     , "aff"
                                     , "colors"
                                     , "css"
                                     , "debug"
                                     , "dom-indexed"
                                     , "effect"
                                     , "halogen"
                                     , "halogen-css"
                                     , "halogen-components"
                                     , "media-types"
                                     , "school-of-music"
                                     , "soundfonts" ],
  packages = ./polyphonic-player-packages.dhall
}
