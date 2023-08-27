module Main where

import LambdaSound

song :: Sound T Pulse
song = cache $ reverb 0.1 $ background <> melody

upwards :: Sound T Pulse
upwards = setDuration 2.5 $ sequentially $ note <$> [c4, d4, e4, f4, g4]

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

main :: IO ()
main = do
  play 44000 song

metronome :: Sound T Pulse
metronome = repeatSound 20 $ note c4 >>> setDuration 1 silence

speedup :: Sound T Pulse -> Sound T Pulse
speedup = changeTempo $ \p -> p ** 2
