module Main where

import Data.Coerce (coerce)
import LambdaSound

main :: IO ()
main = play 44100 0.4 $ (mempty  >>> mempty) >>> note d4

song :: Sound T Pulse
song = melody <> reduce 1.5 background

background :: Sound T Pulse
background =
  repeatSound 4 $
    setDuration 2 $
      sequentially
        [ setDuration 1 $ note c3,
          repeatSound 3 $ parallel $ note <$> [e3, g3]
        ]

melody :: Sound T Pulse
melody =
  let mel =
        repeatSound 3 $
          setDuration 2 $
            sequentially
              [ note c4,
                note e4,
                note g4,
                note e4
              ]
      end = setDuration 2 $ note c4
   in mel >>> end

note :: Semitone -> Sound T Pulse
note st = setDuration 1 $ easeInOut 4 $ asNote harmonic st

-- Further examples

metronome :: Sound T Pulse
metronome = repeatSound 10 $ setDuration 1 $ note c4 >>> setDuration 2 silence

upSound :: Sound T Pulse
upSound =
  zipSoundWith (*) ((\p -> 1 - coerce p) <$> progress) $
    speedUp $
      upwards >>> takeSound 2 (raiseSemitones 12 upwards) >>> setDuration 1 (note g5)

upwards :: Sound T Pulse
upwards = setDuration 3.5 $ sequentially $ note <$> [c4, d4, e4, f4, g4, a4, b4]

speedUp :: Sound T Pulse -> Sound T Pulse
speedUp = changeTempo $ \p -> p ** 2
