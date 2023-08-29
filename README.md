# LambdaSound

A Haskell libary for generating low-level sounds with high-level combinators.
You can create sounds by defining an audio signal via a mathematical function and
then use combinators like `parallel`, `sequentially` or `dropSound` to manipulate them.

## Examples

```haskell
-- An infinite 440hz sinus curve
sound440Hz :: Sound I Pulse
sound440Hz = pulse 440 

-- Three infinite sounds in parallel
triad :: Sound I Pulse
triad = parallel $ fmap (asNote pulse) [c4, e4, g4]

-- Five sequential 1 second sounds 
ascending :: Sound T Pulse
ascending = sequentially $
  fmap (setDuration 1 . asNote pulse) [c4,d4,e4,f4,g4]

-- You can cut apart sounds with takeSound and dropSound
ascendingPart :: Sound T Pulse
ascendingPart = takeSound 1 $ dropSound 1 ascending

-- Add a quiet noise to a sound
noisyAscending :: Sound T Pulse
noisyAscending = parallel
  [ setDuration (getDuration ascending) (reduce 3 (noise 42)),
    ascending
  ]

-- Raise the frequency of a sound so it has a higher pitch
ascendingAnOctaveHigher :: Sound T Pulse
ascendingAnOctaveHigher = raise 8 ascending 

-- Reverse the samples in a sound
descending :: Sound T Pulse
descending = reverseSound ascending

-- Change the tempo the parts of a sound are played at
speedupDuringSound :: Sound d Pulse -> Sound d Pulse
speedupDuringSound = changeTempo $ \progress -> progress ** 1.2

-- Play sound with a sample rate of 44000
main :: IO ()
main = do
  let volume = 0.5
      sampleRate = 44100
  play sampleRate volume ascending
```

You can also take a look at `example/Main.hs` for a bigger example and play it with `cabal run example`.

## Current Features

- [x] Parallel combinator
- [x] Sequence combinator
- [x] Zip combinator
- [x] Volume combinators
- [x] Semitone combinators
- [x] Whole sound combinators
- [x] Convolution
- [x] Scaling speed
- [x] take/drop for sounds
- [x] More Semitones
- [x] Better caching

## Wanted Features

- [ ] More effects
- [ ] More and better samples (especially ones similar to instruments)

## Building

`lambdasound` can be built as usual with the `cabal` package manager. For playing sounds, you will need to have **SDL2** installed.

```
git clone https://github.com/Simre1/lambdasound
cabal build lambdasound
```

You can run the example with:
```
cabal run example
```

## Contributing

Feel free to try out this library and add additional functionality.