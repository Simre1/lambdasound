module Main where

import LambdaSound

main :: IO ()
main =
  let !floats = sampleSound 44100 $ repeatSound 5 song
   in pure ()

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
