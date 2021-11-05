# tidal-listener
Experimental tidal OSC listener.

This is a work-in-progress and the below is not yet implemented.

Basic protocol ideas (`>`, incoming message `<`, outgoing message)

```
> /ping
< /pong
```
run code, get ok or errors back
```
> /code <id> <source>
< /code/ok
```
or
```
< /code/error <id> <error message>
```
Set a name (optional, doesn't have to be unique)
```
> /name <name>
< /name/ok
```
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
get current cps
```
> /cps
< /cps <number>
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