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
          bench "long Sound" $ nfSound longSound
        ]
    ]

nfSound :: Sound T Pulse -> Benchmarkable
nfSound = nfIO . sampleSound 44100

simplePulse :: Sound T Pulse
simplePulse = 3 |-> pulse 440

simpleHarmonic :: Sound T Pulse
simpleHarmonic = 3 |-> harmonic 440

someSounds :: Sound T Pulse
someSounds =
  sequentially
    [ 1 |-> parallel [harmonic 440, harmonic 500, harmonic 1000],
      1 |-> harmonic 200,
      1 |-> harmonic 2000
    ]

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
longSound = repeatSound 5 someSounds
