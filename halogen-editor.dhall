let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "halogen-editor/**/*.purs" ],
  dependencies = conf.dependencies # [ "abc-parser"
                                     , "console"
                                     , "effect"
                                     , "halogen"
                                     , "halogen-components"
                                     , "soundfonts" ],
  packages = ./halogen-editor-packages.dhall
}
