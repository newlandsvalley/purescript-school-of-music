{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "school-of-music"
, dependencies =
  [ "effect"
  , "foldable-traversable"
  , "math"
  , "midi"
  , "rationals"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
