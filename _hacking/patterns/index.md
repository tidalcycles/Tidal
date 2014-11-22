---
category: patterns
weight: -9
---
### what is a Pattern?

A pattern describes something that varies with time. Time is the unit interval.

In ghci, type `"bd sn sn sn" :: Pattern String` (the type is needed for disambiguation of the overloaded string literal) which gives

```haskell
[((0 % 1,1 % 4),"bd"),((1 % 4,1 % 2),"sn"),((1 % 2,3 % 4),"sn"),((3 % 4,1 % 1),"sn")]
```

this shows that during the first quarter of the period, the "bd" sample will be played, and in the following three quarters, the "sn" sample.

### how to find outr what a pattern transformer does

these are your sources of information:

* the manual (pdf on tidal home page) <https://github.com/yaxu/Tidal/raw/master/doc/tidal.pdf>
* API docs and source code (on hackage: <http://hackage.haskell.org/package/tidal-0.2.7/docs/Sound-Tidal-Strategies.html>, locally: run `cabal haddock --hyperlink-source` and open the HTML page in your browser)
* look at the transformer's textual output in ghci

E.g., the `brak` transformer

* manual says "Make a pattern sound a bit like a breakbeat" but this does not really explain it
* source code says `brak = every 2 (((1%4) <~) . (\x -> cat [x, silence]))`
* typing `brak "bd sn sn sn" :: Pattern String` in ghci says

```haskell
[((1 % 4,3 % 8),"bd"),((3 % 8,1 % 2),"sn"),((1 % 2,5 % 8),"sn"),((5 % 8,3 % 4),"sn")]
```


### there are two kinds of patterns, really

there are probably more, but most often we use `Pattern String` (e.g., `"bd sn sn sn" :: Pattern String`)
and `OscPattern` which is actually `Pattern OscMap`. You send `OscPattern` to the backend (it is the argument type for the `d1` function). For conversion, use

```haskell
sound :: Pattern String -> OscPattern
```

Typical usage is `d1 $ sound "bd"` Note that `"bd"` is an overloaded String literal and its actual type is `Pattern String`. Most of the pattern transformers are generic, like 

```haskell
rev :: Pattern a -> Pattern a
```

but some are specific, like 

```haskell
interlace :: OscPattern -> OscPattern -> OscPattern
```

### it is a DSL, and it is an EDSL

embedded DSL: Patterns are Haskell values, and you can combined them by Haskell functions, so the visible syntax is Haskell syntax, e.g.:

```haskell
cat [ "bd" :: Pattern String, "sn" :: Pattern String, "sn" :: Pattern String ]
```
    
DSL: Tidal also has concrete syntax that allows to write longer strings describing patterns. They will be processed by a parser that inserts calls to pattern combinators, e.g.:

```haskell
"bd sn sn" :: Pattern String
```

both above expressions describe the same value (you can check with ghci)

```haskell
[((0 % 1,1 % 3),"bd"),((1 % 3,2 % 3),"sn"),((2 % 3,1 % 1),"sn")]
```
The mapping from concrete to abstract syntax is only given in the source (it seems). For instance, to find out that `"bd sn sn"` really uses `cat`, look here: <http://hackage.haskell.org/package/tidal-0.2.7/docs/src/Sound-Tidal-Parse.html#pSequenceN> 
