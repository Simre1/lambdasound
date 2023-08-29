module Main where

import LambdaSound

main :: IO ()
main = do
  let sampleRate = 44100
      volume = 0.4
  play sampleRate volume song

song :: Sound T Pulse
song = cache $ melody <> background 

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

upwards :: Sound T Pulse
upwards = speedUp $ setDuration 2.5 $ sequentially $ note <$> [c4, d4, e4, f4, g4]

metronome :: Sound T Pulse
metronome = repeatSound 10 $ setDuration 1 $ note c4 >>> setDuration 2 silence

speedUp :: Sound T Pulse -> Sound T Pulse
speedUp = changeTempo $ \p -> p ** 2
