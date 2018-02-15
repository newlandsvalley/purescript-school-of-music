PSoM DSL
========

The DSL is designed to be a natural interface into the PSoM API. It is, however, highly experimental and is likely to change. It has the following syntax:

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
