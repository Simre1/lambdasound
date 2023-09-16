# LambdaSound

A Haskell libary for generating low-level sounds with high-level combinators.

You can create sounds as a list of floats and then manipulate them with 
combinators like `parallel`, `sequentially` or `dropSound`.

## Examples

```haskell
-- An infinite 440hz sinus curve
sound440Hz :: Sound I Pulse
sound440Hz = sineWave 440 

-- Three infinite sounds in parallel
triad :: Sound I Pulse
triad = parallel $ fmap (asNote sineWave) [c4, e4, g4]

-- Five sequential 1 second sounds 
ascending :: Sound T Pulse
ascending = sequentially $
  fmap (setDuration 1 . asNote sineWave) [c4,d4,e4,f4,g4]

-- Cut apart sounds with takeSound and dropSound
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

-- Play sound with a sample rate of 44100
main :: IO ()
main = do
  let volume = 0.5
      sampleRate = 44100
  play sampleRate volume ascending
```

You can also take a look at `example/Example1.hs` and `example/Example2.hs` for bigger examples and play them with:

```haskell
cabal run example1
cabal run example2
```

## Feature Overview

- Play sounds with SDL2
- Save sounds as WAV
- Create raw audio samples by defining a vector of floats
- Manipulate the duration of a sound
- Combine sounds via `parallel`, `sequentially` or `zipSound`
- Change volume
- Modify the pitch
- Create a sound and then map over its samples
- Convolve sounds
- IIR filters
- Cut apart sounds with `takeSound` and `dropSound`
- Scaling playing speed
- Cache expensive to compute sounds in your XDG-cache directory
- Loading wav files (with some caveats)
- Embed IO into sounds

## Building

`lambdasound` can be built as usual with the `cabal` package manager. 

```
git clone https://github.com/Simre1/lambdasound
cabal build lambdasound
```

You can run the example with:

```haskell
cabal run example1
cabal run example2
```

For this library, you will need to have **SDL2** installed. Take a look at [proteeaudio-sdl](https://hackage.haskell.org/package/proteaaudio-sdl) for installation instructions.


## Contributing

Feel free to try out this library and add additional functionality.