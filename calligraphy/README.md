made with this from the top level tidal folder:

```
cabal clean
cabal build --ghc-options=-fwrite-ide-info
calligraphy --hide-data --hide-local-bindings  -s calligraphy/pattern.svg Sound.Tidal.Pattern
calligraphy --hide-data --hide-local-bindings  -s calligraphy/core.svg Sound.Tidal.Core
calligraphy --hide-data --hide-local-bindings  -s calligraphy/ui.svg Sound.Tidal.UI
calligraphy --hide-data --hide-local-bindings  -s calligraphy/control.svg Sound.Tidal.Control
```

