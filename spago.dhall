{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "school-of-music"
, dependencies =
  [ "arrays"
  , "control"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "midi"
  , "rationals"
  , "string-parsers"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
