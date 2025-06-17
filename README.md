Purescript School of Music (PSoM)
=================================

[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-school-of-music.svg)](https://github.com/newlandsvalley/purescript-school-of-music/releases)

[![Build Status](https://github.com/newlandsvalley/purescript-school-of-music/workflows/CI/badge.svg)](https://github.com/newlandsvalley/purescript-school-of-music/actions)

Try it out [here](https://www.tradtunedb.org.uk:8600/).

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

The editor has been developed using Halogen and allows you to enter PSoM text and will parse the text after every keystroke. If it is valid, a player will appear, otherwise an error message is shown. On startup, it loads a pre-selected set of instrument soundfonts which you can later change if you prefer. It also allows you to import an ABC file as PSoM and to load or save the PSoM text.

Polyphonic ABC Player
---------------------

This is now deprecated in favour of [share-a-tune](https://github.com/newlandsvalley/share-a-tune) which has used this code as a starting point and then added tune URL sharing facilities.s

The player is essentially identical to the editor in that, when the ABC is loaded, the tune can be played and the score viewed. Monophonic tunes just play normally. However, if the tune is polyphonic, the user can choose to play the entire polyphonic melody or just one of the parts. The scores for the various parts are displayed separately.

Because the transformation to PSoM is relatively expensive, there is no editor window. It would no longer be possible to update the melody and the score after each keystroke and have reasonable responsiveness. 

To build
--------

     mpm run build

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


