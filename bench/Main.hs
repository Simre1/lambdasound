module Main where

import Data.Coerce
import LambdaSound
import Test.Tasty.Bench
import qualified Data.Massiv.Array as M

main :: IO ()
main =
  defaultMain
    [ bgroup
        "LambdaSound"
        [ bench "Simple Pulse" $ nfSound simplePulse,
          bench "Simple Harmonic" $ nfSound simpleHarmonic,
          bench "Some Sounds" $ nfSound someSounds,
          bench "Noise" $ nfSound noiseSound,
          bench "Convolution" $ nfSound convolutionSound,
          bench "Long Sound" $ nfSound longSound,
          bench "Filtered sound" $ nfSound filteredSound,
          bench "Dropped sound" $ nfSound droppedSound,
          bench "Taken sound" $ nfSound takenSound,
          bench "Cached sound" $ nfSound cachedSound,
          bench "Timed parallel sound" $ nfSound timedParallelSound,
          bench "Unfold pulse" $ nfSound unfoldPulse,
          bench "Unfold normally" $ nfSound unfoldNormally,
          bench "With sampled sound" $ nfSound useSampledSound,
          bench "Repeated with sampled sound" $ nfSound repeatedSampledSound
        ]
    ]

nfSound :: Sound T Pulse -> Benchmarkable
nfSound = nfIO . sampleSound 44100

simplePulse :: Sound T Pulse
simplePulse = 3 |-> sineWave 440

simpleHarmonic :: Sound T Pulse
simpleHarmonic = 3 |-> harmonic sineWave 440

someSounds :: Sound T Pulse
someSounds =
  sequentially
    [ 1 |-> parallel [harmonic sineWave 440, harmonic sineWave 500, harmonic sineWave 1000],
      1 |-> harmonic sineWave 200,
      1 |-> harmonic sineWave 2000
    ]

filteredSound :: Sound T Pulse
filteredSound = applyIIRFilter (highPassFilter 1000 1) someSounds

noiseSound :: Sound T Pulse
noiseSound = 3 |-> noise 42

convolutionSound :: Sound T Pulse
convolutionSound =
  convolveDuration
    ( Kernel
        { size = 0.02,
          offset = 0,
          coefficients = coerce
        }
    )
    (1 |-> simplePulse)

longSound :: Sound T Pulse
longSound = repeatSound 20 someSounds

droppedSound :: Sound T Pulse
droppedSound = repeatSound 10 $ dropSound 0.5 someSounds

takenSound :: Sound T Pulse
takenSound = repeatSound 10 $ takeSound 2.5 someSounds

cachedSound :: Sound T Pulse
cachedSound = cache longSound

timedParallelSound :: Sound T Pulse
timedParallelSound =
  parallel $
    mconcat $
      replicate
        5
        [ 0.5 |-> simplePulse,
          1 |-> simplePulse,
          1.5 |-> simplePulse,
          0.7 |-> simplePulse,
          2 |-> simplePulse,
          1.5 |-> simplePulse
        ]

unfoldPulse :: Sound T Pulse
unfoldPulse = 5 |-> unfoldlSoundPulse (\s -> (s, succ s)) 0

unfoldNormally :: Sound T Pulse
unfoldNormally = 5 |-> unfoldlSound (\s -> (s, succ s)) 0

useSampledSound :: Sound T Pulse
useSampledSound = setDuration 5 $ withSampledSoundPulse simplePulse $ \samples ->
  makeSound $ \_ index -> samples M.! (index `mod` M.unSz (M.size samples))

repeatedSampledSound :: Sound T Pulse
repeatedSampledSound = repeatSound 20 useSampledSound