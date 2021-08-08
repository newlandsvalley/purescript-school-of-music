{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}


let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210613/packages.dhall sha256:5f10380b3ca7d3a32ea5c2b7535e4814a5e3f3590c70692f76e596d6ab0687b3
      
in  upstream
  with abc-parser =
    { dependencies = 
    [ "bifunctors"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "maybe"
    , "midi"
    , "ordered-collections"
    , "profunctor-lenses"
    , "rationals"
    , "strings"
    , "stringutils"
    , "string-parsers"
    , "transformers"
    , "tuples"
    ]
    , repo = "https://github.com/newlandsvalley/purescript-abc-parser.git"
    , version = "ps014"
    }
  with halogen-components =
     { dependencies =
         [ "console"
         , "css"
         , "effect"
         , "js-fileio"
         , "halogen"
         , "halogen-css"
         , "soundfonts"
         ]
     , repo =
         "https://github.com/newlandsvalley/purescript-halogen-components.git"
     , version =
         "ps014"
     }
  with abc2psom =
     { dependencies =
         [ "abc-parser", "school-of-music", "soundfonts" ]
     , repo =
         "https://github.com/newlandsvalley/purescript-abc2psom.git"
     , version =
         "ps014"
     }
  with halogen-css =
      { dependencies =
          [ "css"
          , "halogen"
          ]
      , repo =
          "https://github.com/newlandsvalley/purescript-halogen-css.git"
      , version =
          "master"
      }
  with school-of-music = 
    ./spago.dhall as Location
