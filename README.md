# LambdaSound

A Haskell libary for generating sounds. Sounds are just a series of floats which can then be played as music.

## Examples

```haskell
sound440Hz :: Sound I Pulse
sound440Hz = pulse 440 

triad :: Sound I Pulse
triad = parallel $ fmap (asNote pulse) [c4, e4, g4]

ascending :: Sound T Pulse
ascending = sequentially $
  fmap (setDuration 1 . asNote pulse) [c4,d4,e4,f4,g4]

ascendingPart :: Sound T Pulse
ascendingPart = takeSound 1 $ dropSound 1 ascending

noisyAscending :: Sound T Pulse
noisyAscending = parallel
  [ setDuration (getDuration ascending) (reduce 3 (noise 42)),
    ascending
  ]

ascendingAnOctaveHigher :: Sound T Pulse
ascendingAnOctaveHigher = raise 8 ascending 

descending :: Sound T Pulse
descending = reverseSound ascending

speedupDuringSound :: Sound d Pulse -> Sound d Pulse
speedupDuringSound = changeTempo $ \progress -> progress ** 1.2
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