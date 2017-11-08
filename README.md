Purescript School of Music (PSoM)
=================================

[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-school-of-music.svg)](https://github.com/newlandsvalley/purescript-school-of-music/releases)
[![Build Status](https://travis-ci.org/newlandsvalley/purescript-school-of-music.svg?branch=master)](https://travis-ci.org/newlandsvalley/purescript-school-of-music)

Try it out [here](http://www.tradtunedb.org.uk:8600/).

This is another attempt at porting the music notation part of the [Haskell School of Music](https://github.com/Euterpea/Euterpea2) (HSoM) to the browser. It consists of a PSoM library (ported from HSoM) together with an editor that runs in the browser. This allows you to enter melodies using a DSL (intended to be a natural interface into the PSoM API) and then play them.

It follows an [abortive attempt in Elm](https://github.com/danigb/elm-school-of-music) in conjunction with danigb.  This failed largely because of the lack of type classes in Elm but also because of the time delays inherent in Elm's port system when requesting that a sound should actually be played.

Supported Instruments
---------------------

PSoM uses instruments from [Benjamin Gleitzman's soundfont library](https://github.com/gleitz/midi-js-soundfonts) which are enumerated in [purescript-midi](https://github.com/newlandsvalley/purescript-midi).  It recognizes all the instruments listed [here](http://gleitz.github.io/midi-js-soundfonts/FluidR3_GM/names.json).  The names differ slightly from those originally used by HSoM - the mapping between the two is shown [here](https://github.com/newlandsvalley/purescript-school-of-music/blob/master/HSoM_INSTRUMENTS.md).  MIDI allows up to 10 such instruments to be available for any given melody.

DSL
---

Melodies are presented to the browser using a DSL with the following syntax:

``` 
    psom = title musicProcedure

    musicProcedure = complexMusic | music
    
    complexMusic = 'Let' bindings 'In' music
    
    bindings = binding, { binding }
    
    binding = identifier '=' music

    music = voices | lines | line | chord | prim | control "(" music ")"

    voices = 'Par' musicProcedure, { musicProcedure }

    lines = 'Seq' seqOptions, { seqOptions }

    line = 'Line' lineOptions, { lineOptions }
    
    seqOptions = line | variable | control

    lineOptions = chord | prim | control

    chord = 'Chord' '[' prim, { prim } ']'

    prim = note | rest

    note = 'Note' dur pitch 
    
    variable = identifier

    rest = 'Rest' dur

    dur = 'wn'| 'hn' |'qn'| 'sn' ......

    pitch = pitchClass octave

    pitchClass = 'Cff' | 'Cf' | 'C' | 'Cs' | 'Css' | 'Dff' .....

    octave = int
    
    title = quoted string
    
```
where control mechanisms are:

```
   control =   'Instrument' instrumentName 
             | 'Transpose' int 
             | 'Tempo' (fraction | int) 
             | 'PhraseAtts' phraseAttributes

   phraseAttributes = phraseAttribute, ( phraseAttribute }
   
   phraseAttribute =   'Loudness' int
                     | 'StdLoudness' ( FFF | F | .....
                     | 'Diminuendo' ( fraction | int ) 
                     | 'Crescendo' ( fraction | int ) 
                     | 'Accent' ( fraction | int ) 
                     | 'Ritardando' ( fraction | int ) 
                     | 'Accelerando' ( fraction | int ) 
                     | 'Staccato' ( fraction | int ) 
                     | 'Legato' ( fraction | int ) 
                     | 'Slurred' ( fraction | int ) 
                     

    instrumentName = 'violin' | 'viola' ....                     
                     
```

All keywords start with an upper-case letter.  Variables (which represent a repeatable section of music) start with a lower-case letter. Comments are supported after the title or after the _Par_ and _Seq_ keywords. The DSL attempts to give a convenient representation for lines of music and chords, whilst still retaining the ability to control whole phrases (however built). It is very experimental and likely to change. As far as I am aware, all features of the Music ADT which are fully supported by HSoM have been carried across to the DSL.

See the DSL tests and editor examples for example usage.

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


