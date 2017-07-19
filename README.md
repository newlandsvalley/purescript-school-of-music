Purescript School of Music (PSoM)
=================================

WORK IN PROGRESS

This is another attempt at porting the music notation part of the [Haskell School of Music](https://github.com/Euterpea/Euterpea2) (HSoM) to the browser. It follows an [abortive attempt in Elm](https://github.com/danigb/elm-school-of-music) in conjunction with danigb.  This failed largely because of the lack of type classes in Elm but also because of the time delays inherent in Elm's port system when requesting that a sound should actually be played.

It consists of a PSoM library, ported from HSoM together with an editor that runs in the browser. This allows you to enter melodies using a DSL which attempts to be a simple interface to the PSoM API.

You can try it out [here](http://www.tradtunedb.org.uk:8600/).

Current State of Progress
-------------------------

The editor is built using polyphonic soundfonts which must be pre-loaded for selected instruments before anything plays.  The PSoM score is translated to a Melody (as accepted by the [MIDI player](https://github.com/newlandsvalley/purescript-midi-player)) which is an interruptible series of MIDI phrases.  Interruption is only enacted at a phrase boundary, and so it will take a noticeable time for the current phrase to end before taking effect. 

Supported Instruments
---------------------

PSoM uses instruments from [Benjamin Gleitzman's soundfont library](https://github.com/gleitz/midi-js-soundfonts).  It recognizes all the instruments listed [here](http://gleitz.github.io/midi-js-soundfonts/FluidR3_GM/names.json).  These differ slightly from the names originally used by HSoM - the mapping between the two is shown [here](https://github.com/newlandsvalley/purescript-school-of-music/blob/master/HSoM_INSTRUMENTS.md).  MIDI allows up to 10 such instruments to be available for any given melody.

Front End
---------

PSoM melodies are presented to the browser using a DSL with the following syntax:

```    
    musicProcedure = complexMusic | music
    
    complexMusic = 'Let' bindings 'In' music
    
    bindings = binding, { binding }
    
    binding = identifier '=' music

    music = voices | lines | line | chord | prim | control music

    voices = 'Par' musicProcedure, { musicProcedure }

    lines = 'Seq' 'lineOrVariable, { lineOrVariable }

    line = 'Line' chordorprim, { chordorprim }

    chordorprim = chord | prim

    chord = 'Chord' '[' prim, { prim } ']'

    prim = note | rest

    note = 'Note' dur pitch vol
    
    variable = identifier

    rest = 'Rest' dur

    dur = 'wn'| 'hn' |'qn'| 'sn' ......

    pitch = pitchClass octave

    pitchClass = 'Cff' | 'Cf' | 'C' | 'Cs' | 'Css' | 'Dff' .....

    octave = int

    control = 'Instrument' instrumentName | 'Transpose' int

    instrumentName = 'violin' | 'viola' ....
```

All keywords start with an upper-case letter.  Variables (which represent a repeatable section of music) start with a lower-case letter.

See the DSL tests for example usage.

This attempts to give a convenient representation for lines of music and chords, whilst still retaining the ability to control whole phrases (however built). If this works out, we can then build in the further control mechanisms that are supported by the API.

The DSL is very experimental and likely to change.  

Back End
--------

A PSoM melody is converted (via PSoM's __MEvent__) into a [MIDI Melody](https://github.com/newlandsvalley/purescript-midi-player/blob/master/src/Data/Midi/Player/HybridPerformance.purs). This supports up to 10 channels, each dedicated to a particular MIDI instrument.  This is then played using  [purescript-polyphonic-soundfonts](https://github.com/newlandsvalley/purescript-polyphonic-soundfonts) .

Editor
------

The __editor__ sub-project is an editor for music written with the Euterpea DSL.  At the moment, this parses any DSL text and either displays an error or else the results of converting it to a PSoM Performance. It checks the instrument names in entered into the DSL and associates each with the MIDI channel for that instrument (if loaded) or to channel 0 (if not). It then allows the melody to be played.

Design Questions
----------------

### DSL

What features would make the DSL pleasant and convenient to use?

## Soundfonts

What options should we give the user for (re-)loading soundfonts?

To Do
-----

*  Add the other control mechanisms to the DSL
*  Load and save scores
*  Don't translate to MEvent until 'play' is first pressed
*  Add option to re-select the loaded soundfonts
*  Add quickcheck style tests to the PSoM library


