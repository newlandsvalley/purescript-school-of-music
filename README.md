Purescript School of Music
==========================

WORK IN PROGRESS

This is another attempt at porting the music notation part of the [Haskell School of Music](https://github.com/Euterpea/Euterpea2) (HSoM) to the browser. It follows an [abortive attempt in Elm](https://github.com/danigb/elm-school-of-music) in conjunction with danigb.  This failed largely because of the lack of type classes in Elm but also because of the time delays inherent in Elm's port system when requesting that a sound should actually be played.

Partly this project is about helping me to understand the differences between Purescript and Haskell.  But I am hopeful that at least we might be able to express pieces of music using the HSoM API and hear them playing in the browser.

Front End
---------

How should we input HSoM melodies to the browser?  It seems to me the best way would be to construct an HSoM DSL with syntax similar to the following:

```
    polyphony = music | voices

    voices = 'Par' music, { music }

    music = prim | line | lines | repeat | chord | control music

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

Back End
--------

HSoM melodies will be converted (via HSoM's __MEvent__) into [MIDI](https://github.com/newlandsvalley/purescript-midi). This supports up to 10 channels, each of which can be dedicated to a MIDI instrument.  It should then be possible to play the midi using [purescript-soundfonts](https://github.com/newlandsvalley/purescript-soundfonts) although this will need to be extended (together with the MIDI processing capabilities) in order to cater for more than one instrument.

Editor
------

The __editor__ sub-project is an editor for music written with the Euterpea DSL.  At the moment, this parses any DSL text and either displays an error or else the results of converting it to an HSoM Performance.  Gradually I intend to improve this so that eventually it will include a player for polyphonic performances.

Design Questions
----------------

### Instruments

If we are running in the browser, we no longer have a comprehensive set of instrument soundfonts automatically at hand.  Instead, we must explicitly load the soundfonts that we need and this takes time.  Whereas HSoM allows each voice to be labelled with an instrument (as indicated by the DSL above) perhaps this is not a good approach to carry over into PSoM.  Instead, a browser application might first load a set of (up to ten) instrument soundfonts and these would be implicitly associated with each channel (as indicated by each top-level Par voice). A user could then alter the instrumentation by loading a different set of soundfonts.


