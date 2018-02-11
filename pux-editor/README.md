PSoM Editor
===========

The __pux-editor__ example pre-loads a set of instrument soundfonts, the first such being the default. You have the choice of replacing them at any time. It parses the DSL text after each keystroke and checks each instrument name against those that have been loaded.  If an instrument is not mentioned, or its sounfont has not been loaded, the default is used. Any errors in the DSL text are displayed, otherwise, the player is made visible,  If you press play, the parsed tune is converted to a Melody which is an interruptible series of MIDI phrases. It then plays the melody using the appropriate soundfonts. Interruption is only enacted at a phrase boundary, and so it will take a noticeable time for the current phrase to end before taking effect.

It supports loading and saving of PSoM melodies and also the import of ABC text.

### to build 

   cd to editor
 
   bower install

   ./build.sh

   and then navigate to dist/index.html
