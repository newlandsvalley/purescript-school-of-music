Purescript School of Music (PSoM)
=================================

[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-school-of-music.svg)](https://github.com/newlandsvalley/purescript-school-of-music/releases)

[![Build Status](https://github.com/newlandsvalley/purescript-school-of-music/workflows/CI/badge.svg)](https://github.com/newlandsvalley/purescript-school-of-music/actions)

Try it out [here](http://www.tradtunedb.org.uk:8600/).

This is another attempt at porting the music notation part of the [Haskell School of Music](https://github.com/Euterpea/Euterpea2) (HSoM) to the browser. It consists of a PSoM library (ported from HSoM) together with an editor that runs in the browser. This allows you to enter melodies using a DSL and then play them.

It follows an [abortive attempt in Elm](https://github.com/danigb/elm-school-of-music) in conjunction with danigb.  This failed largely because of the lack of type classes in Elm but also because of the time delays inherent in Elm's port system when requesting that a sound should actually be played.

Supported Instruments
---------------------

PSoM uses instruments from [Benjamin Gleitzman's soundfont library](https://github.com/gleitz/midi-js-soundfonts) which are enumerated in [purescript-midi](https://github.com/newlandsvalley/purescript-midi).  It recognizes all the instruments listed [here](http://gleitz.github.io/midi-js-soundfonts/FluidR3_GM/names.json).  The names differ slightly from those originally used by HSoM - the mapping between the two is shown [here](https://github.com/newlandsvalley/purescript-school-of-music/blob/master/HSoM_INSTRUMENTS.md).  MIDI allows up to 10 such instruments to be available for any given melody.

DSL
---

Melodies are presented to the browser using a [DSL](https://github.com/newlandsvalley/purescript-school-of-music/blob/master/DSL.md) which has been designed to be as close as possible to the API itself.

Editor
------

The editor hase been developed using Halogen and allows you to enter PSoM text and will parse the text after every keystroke. If it is valid, a player will appear, otherwise an error message is shown. On startup, it loads a pre-selected set of instrument soundfonts which you can later change if you prefer. It also allows you to import an ABC file as PSoM and to load or save the PSoM text.

There is also a deprecated Pux editor and a set of sample tunes using Pux which will not be maintained beyond PureScript 0.11.

Polyphonic ABC Player
---------------------

Try it [here](http://www.tradtunedb.org.uk:8605/).

This player is essentially identical to the editor, except that it allows ABC to be loaded and viewed in the editor window.  If the tune is polyphonic, the user can choose to play the polyphonic melody or just one of the parts and can also see the score of each part.  Monophonic tunes just play normally. 

Because the transformation to PSoM is relatively expensive, we can no longer regenerate the output after each keystroke as we can for the monophonic ABC editor.  For this reason, editing the ABC text is disallowed.

To build
--------

     spago build

or

     bower install
     pulp build

To test
-------

     npm run test 

To build the Halogen editor
---------------------------

     npm run halogen-editor

and then navigate to halogen-editor/dist/index.html

To build the polyphonic player
------------------------------

     npm run polyphonic-player

and then navigate to polyphonic-player/dist/index.html

Design Questions
----------------

### DSL

What features would make the DSL pleasant and convenient to use?

### Volume

What would be a sensible default value for a note volume (currently 100)?

To Do
-----

*  Allow variable definitions to refer to other variables defined earlier in the same scope
*  Add quickcheck style tests to the PSoM library

The following control mechanisms are unimplemented because they also have not been implemented in HSoM:

*  Custom
*  KeySig
*  Ornamentation
*  Articulation other than Staccato, Legato and Slurred
  

Questions on the HSoM Implementation
------------------------------------


There seem to be various problems surrounding volume in MEvent.  Perhaps it is because I am using only a MIDI backend which has a maximum volume setting of 7F (127).  Firstly, crescendos seem to _start_ at this volume level, although diminuendos are OK.  Secondly, the Loudness implementation seems correctly to set the volume in the context but then to ignore it, taking the volume only of the original note.

The various articulations don't seem to work properly.  Not sure yet if it's a bug in HSoM or in the translation to PureScript.


