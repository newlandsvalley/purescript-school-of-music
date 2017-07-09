Purescript School of Music (PSoM)
=================================

WORK IN PROGRESS

This is another attempt at porting the music notation part of the [Haskell School of Music](https://github.com/Euterpea/Euterpea2) (HSoM) to the browser. It follows an [abortive attempt in Elm](https://github.com/danigb/elm-school-of-music) in conjunction with danigb.  This failed largely because of the lack of type classes in Elm but also because of the time delays inherent in Elm's port system when requesting that a sound should actually be played.

Current State of Progress
-------------------------

The editor is built using the monophonic version of soundfonts.  There is a quick-and-dirty translation from a PSoM Performance to a Melody as required by the MIDI player which doesn't allow interruption when playing back.  However, it is now possible to write music using the DSL and to hear the result.


Front End
---------

How should we input PSoM melodies to the browser?  It seems to me the best way would be to construct a DSL with syntax similar to the following:

```    
    music = voices | lines | line | repeat | chord | prim | control music

    voices = 'Par' music, { music }

    lines = 'Seq' 'line, { line }

    line = 'Line' chordorprim, { chordorprim }
    
    repeat = 'Repeat' '(' lines ')'

    chordorprim = chord | prim

    chord = 'Chord' '[' prim, { prim } ']'

    prim = note | rest

    note = 'Note' dur pitch vol

    rest = 'Rest' dur

    dur = 'wn'| 'hn' |'qn'| 'sn' ......

    pitch = pitchClass octave

    pitchClass = 'Cff' | 'Cf' | 'C' | 'Cs' | 'Css' | 'Dff' .....

    octave = int

    control = 'Instrument' instrumentName

    instrumentName = 'violin' | 'viola' ....
```

See the DSL tests for example usage.

This attempts to give a convenient representation for lines of music and chords, whilst still retaining the ability to control whole phrases (however built). If this works out, we can then build in the further control mechanisms that are supported by the API.

The DSL is very experimental and likely to change.  There are all sorts of directions where we might take it - for example to define music 'functions' which would then be called later in the body of the tune.

Back End
--------

PSoM melodies are converted (via PSoM's __MEvent__) into a [MIDI Melody](https://github.com/newlandsvalley/purescript-midi-player/blob/master/src/Data/Midi/Player/HybridPerformance.purs). This supports up to 10 channels, each dedicated to a particular MIDI instrument.  It should then be possible to play the midi using [purescript-polyphonic-soundfonts](https://github.com/newlandsvalley/purescript-polyphonic-soundfonts) although currently the monophonic version is used.

Editor
------

The __editor__ sub-project is an editor for music written with the Euterpea DSL.  At the moment, this parses any DSL text and either displays an error or else the results of converting it to a PSoM Performance, which is then playable. 

Design Questions
----------------

### DSL

What features would make the DSL pleasant and convenient to use?

### Instruments

If we are running in the browser, we no longer have a comprehensive set of instrument soundfonts automatically at hand.  Instead, we must explicitly load the soundfonts that we need and this takes time.  Whereas HSoM allows each voice to be labelled with an instrument (as indicated by the DSL above) perhaps this is not a good approach to carry over into PSoM.  Instead, a browser application might first load a set of (up to ten) instrument soundfonts and these would be implicitly associated with each channel (as indicated by each top-level Par voice). A user could then alter the instrumentation by loading a different set of soundfonts.


