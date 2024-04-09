

Sequences can be pre-aligned before a bind

Signals can't be pre-aligned, but some tricks for alignment during a
bind is possible, e.g. trig, squeeze

Pattern instances need to take care of all this for themselves, by
being responsible for patternifying functions in a way that selects
the right strategy based on their own metadata.

Signals and patterns now have metadata (in a pretty hacky way)

What is left is abstracting patternification

Usually (but not always) the final argument is a pattern

how to turn

_fast :: Time -> p a -> p a
into
fast :: p Time -> p a -> p a

Sequences
- step 1: align the two patterns
- step 2: bind

Signals
- bind

No step 1 as they can't be aligned, so 1 cycle is always the reference point

For higher arities..

_every :: Time -> (p a -> p a) -> p a -> p a
every :: p Time -> p (p a -> p a) -> p a -> p a

go backwards?

- align last two, then second from last with result etc
- bind

This works, somehow:

```
bindify_p_n_n $ (bindify_p_n <$> _every)
```
