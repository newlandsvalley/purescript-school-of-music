Purescript School of Music (PSoM)
=================================

WORK IN PROGRESS

This is another attempt at porting the music notation part of the [Haskell School of Music](https://github.com/Euterpea/Euterpea2) (HSoM) to the browser. It follows an [abortive attempt in Elm](https://github.com/danigb/elm-school-of-music) in conjunction with danigb.  This failed largely because of the lack of type classes in Elm but also because of the time delays inherent in Elm's port system when requesting that a sound should actually be played.

Current State of Progress
-------------------------

The editor is built using polyphonic soundfonts which must be pre-loaded for selected instruments before anything plays.  The PSoM score is translated to a Melody (as accepted by the [MIDI player](https://github.com/newlandsvalley/purescript-midi-player)) which is an interruptible series of MIDI phrases.  Interruption is only enacted at a phrase boundary, and so it will take a noticeable time for the current phrase to end before taking effect. 

Supported Instruments
---------------------

PSoM uses instruments from [Benjamin Gleitzman's soundfont library](https://github.com/gleitz/midi-js-soundfonts).  It recognizes all the instrument names as listed [here](http://gleitz.github.io/midi-js-soundfonts/FluidR3_GM/names.json).  These differ slightly from the names originally used by HSoM - the mapping between the two is shown [here](https://github.com/newlandsvalley/purescript-school-of-music/blob/master/HSoM_INSTRUMENTS.md).  MIDI allows up to 10 such instruments to be available for any given melody.

Front End
---------

How should we input PSoM melodies to the browser?  It seems to me the best way would be to construct a DSL with syntax similar to the following:

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

    control = 'Instrument' instrumentName

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

The __editor__ sub-project is an editor for music written with the Euterpea DSL.  At the moment, this parses any DSL text and either displays an error or else the results of converting it to a PSoM Performance, which is then playable. 

Design Questions
----------------

### DSL

What features would make the DSL pleasant and convenient to use?

## Soundfonts

What options should we give the user for (re-)loading soundfonts?

What should be the behaviour if the melody description mentions a soundfont which is not currently loaded?  Perhaps default to the first soundfont (channel 0)?


