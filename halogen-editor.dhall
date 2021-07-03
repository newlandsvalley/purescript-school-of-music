let conf = ./spago.dhall

in conf // {
  sources = [ "halogen-editor/**/*.purs" ],
  dependencies = conf.dependencies # [ "abc2psom"
                                     , "abc-parser"
                                     , "aff"
                                     , "colors"
                                     , "css"
                                     , "dom-indexed"
                                     , "effect"
                                     , "halogen"
                                     , "halogen-components"
                                     , "halogen-css"
                                     , "js-fileio"
                                     , "media-types"
                                     , "school-of-music"
                                     , "soundfonts" ],
  packages = ./halogen-editor-packages.dhall
}
