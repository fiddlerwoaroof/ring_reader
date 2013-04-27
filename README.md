# Ring Reader -- a pager-like "ring buffer"

[![Build Status](https://travis-ci.org/fiddlerwoaroof/ring_reader.png)](https://travis-ci.org/fiddlerwoaroof/ring_reader)


This program takes a command line to run and prints the output of that command
to the screen.  When the output reaches the bottom of the screen, it wraps to 
the top, thus saving scrollback memory. It was inspired by wanting to have a way
to keep an eye on the results of `./ring_reader 
~/android_sdks/platform-tools/adb logcat` without having it constantly printing 
to the terminal.

## Dependencies

- Haskell (tested with The Glorious Glasgow Haskell Compilation System, version 
  7.4.1)

- HSCurses
- CmdArgs

## Options

- '-i' -- wait for keypress after each screenful.

