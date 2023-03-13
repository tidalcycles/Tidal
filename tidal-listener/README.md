# tidal-listener
Experimental tidal OSC listener.

## Install

Move to the repository directory and run `cabal install`.

On Linux systems, the `tidal-listener` binary will be found inside `~/.cabal/bin/`.

There are some command line options to set the listening, reply and dirt port as well as to specify the mode of the listener. There is a mode that assumes that GHC and Tidal are installed on the system, the other mode makes no such assumption but requires a specific folder of additional files. Unfortunately, this mode is currently broken and in progress of being fixed, see
https://github.com/haskell-hint/hint/issues/156

```
Usage: tidal-listener [-l|--listenport INT] [-r|--replyport INT]
                      [-d|--dirtport INT] [--no-ghc]

  An OSC interpreter for TidalCycles

Available options:
  -l,--listenport INT      Specify the listening port (default: 6011)
  -r,--replyport INT       Specify the reply port (default: 6012)
  -d,--dirtport INT        Specify the dirt port (default: 5720)
  --no-ghc                 If this flag is active, the interpreter will assume
                           that GHC not installed on the system
  -h,--help                Show this help text
```

## Protocol

This is a work-in-progress and the below is not yet implemented.

Basic protocol ideas (`>`, incoming message `<`, outgoing message)

```
> /ping
< /pong
```
run statements, get ok, a value or errors back
```
> /eval <statement>
< /eval/ok
```
or
```
< /eval/value <value>
```
or
```
< /eval/error <error message>
```

get the type of an expression (or an error)
```
> /type <expression>
< /type/ok <type>
```
or
```
< /type/error <error>
```

load a file at a given path
```
> /load <path>
< /load/ok
```
or
```
< /load/error <error>
```

get current cps
```
> /cps
< /cps <number>
```

## Speculative, not implemented yet

'Expand' an expression into canonical mininotation, ref https://github.com/tidalcycles/Tidal/issues/633
```
< /expand <code>
> /expand/ok <expanded code>
```
Set port listening to replies (if not the sending port)
```
> /port <number>
< /port/ok
```
Set highlights on, get stream of active code spans+durations back (or set it off again)
```
> /highlights/on
< /highlights/on ok
> /highlights/off
< /highlights/off ok
< /code/highlight <pattern id> <duration> <cycle position> <col> <row> <col> <row>
```
set cps
```
> /cps/set <number>
< /cps/set ok
< /cps <number> - sent to all clients ?
```

Show which patterns are playing/currently active:
```
> /nowplaying/ <d1?>
< /nowplaying/ true/false -- add highlighting to variables currently active?
```


Show events using queryArc -- from https://github.com/tidalcycles/tidal-listener/issues/1
```
> queryArc "some pattern" arcsize
< [((1,1),(2,1)),((1,1),(2,1))|"a",[((1,1),(2,1)),((3,1),(4,1))]0-(½>1)|"b"]
OR
> getEvents 4 8 (s "bd ~ cp/4")
```


Show length of sample -- from https://club.tidalcycles.org/t/ticking-sound-on-splice-and-cps-question/3033
```
> /samplelength/ "bev"
< /samplelength/ 16
```

We probably need a way to add an identifier to incoming commands that gets added to outgoing commands, to help clients match up replies.
