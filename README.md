# LambdaSound

A Haskell libary for generating sounds. Sounds are just a series of floats which can then be played as music.

## Examples

```haskell
sound440Hz :: Sound I Pulse
sound440Hz = pulse 440 

triad :: Sound I Pulse
triad = parallel $ fmap (asNote pulse) [c4, e4, g4]
```

You can also take a look at `app/Main.hs` for a bigger example and play it with `cabal run lambdasound-exe`.

## Current Features

- [x] Parallel combinator
- [x] Sequence combinator
- [x] Zip combinator
- [x] Volume combinators
- [x] Semitone combinators
- [x] Scaling speed
- [x] take/drop for sounds

## Wanted Features

- [ ] More effects
- [ ] Much more and better samples (especially ones similar to instruments)
- [ ] Specific durations (quarter notes, eigth notes, settable BPM)
- [ ] More Semitones
- [ ] Better caching

## Contributing

Feel free to try out this library and add additional functionality.