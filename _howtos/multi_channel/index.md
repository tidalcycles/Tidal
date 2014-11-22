---
category: multi_channel
weight: -9
---
If you have a multichannel sound module, and a lot of speakers, you may wish to use tidal in multichannel mode. Another possibility is that you want to make a multitrack recording, or send a pattern to a headphone channel for previewing.

### Dirt ###

To do this, first you have to start the Dirt synth with more channels than the default of 2 (stereo), and it's also highly recommended to turn the compressor off:

```bash
$ ./dirt --channels 4 --no-dirty-compressor
```

### Tidal ###

From Tidal, you address the different channels with the `pan` synth parameter, for example, this would play on each of the four speakers:

```
sound "numbers:0 numbers:1 numbers:2 numbers:3"
  |+| pan "0 0.25 0.5 0.75"
```

The range of the pan parameter is still from 0 to 1. So, if you had four speakers, 0 would be the first one, 0.5 would be the third, 0.625 would be panned halfway between the third and fourth, and 0.875 would be halfway between fourth and first. 1 would be the same as 0.
