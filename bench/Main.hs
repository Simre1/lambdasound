module Main where

import Data.Coerce
import LambdaSound
import Test.Tasty.Bench

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
          bench "long Sound" $ nfSound longSound,
          bench "modulated sound" $ nfSound modulatedSound
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

modulatedSound :: Sound T Pulse
modulatedSound = someSounds

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
