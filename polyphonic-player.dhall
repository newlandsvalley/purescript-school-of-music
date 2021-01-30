let conf = ./spago.dhall

in conf // {
  sources = {-conf.sources # -} [ "polyphonic-player/**/*.purs" ],
  dependencies = conf.dependencies # [ "abc-parser"
                                     , "abc-scores"
                                     , "abc2psom"
                                     , "console"
                                     , "debug"
                                     , "effect"
                                     , "halogen"
                                     , "halogen-components"
                                     , "soundfonts" ],
  packages = ./polyphonic-player-packages.dhall
}
