# LambdaSound

A Haskell libary for generating sounds. Sounds are just a series of floats which can then be played as music.

## Examples

```haskell
sound440Hz :: Sound Pulse
sound440Hz = pulse 440 1

notes :: Sound Pulse
notes = asNote pulse c4 1 <> asNote pulse e4 1 <> asNote pulse g4 1 
```

You can also take a look at `app/Main.hs` for a bigger example and play it with `cabal run lambdasound-exe`.

## Current Features

- [x] Parallel combinator
- [x] Sequence combinator
- [x] Zip combinator
- [x] Volume combinators
- [x] Semitone combinators
- [x] Scaling speed

## Wanted Features

- [ ] take/drop for sounds
- [ ] More effects
- [ ] Much more and better samples (especially ones similar to instruments)
- [ ] Specific durations (quarter notes, eigth notes, settable BPM)
- [ ] More Semitones
- [ ] Better caching
