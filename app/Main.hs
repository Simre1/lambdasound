module Main where

import LambdaSound

song :: Sound Pulse
song = upwards

upwards :: Sound Pulse
upwards = reverb 0.5 $ setDuration 2.5 $ mconcat $ note <$> [c4, d4, e4, f4, g4]

background :: Sound Pulse
background =
  repeatSound 4 $
    setDuration 2 $
      mconcat
        [ parallel $ note <$> [c3],
          repeatSound 3 $ parallel $ note <$> [e3, g3]
        ]

melody :: Sound Pulse
melody =
  let mel =
        repeatSound 3 $
          setDuration 2 $
            mconcat
              [ note c4,
                note e4,
                note g4,
                note e4
              ]
      end = setDuration 2 $ note c4
   in mel <> end

note :: Semitone -> Sound Pulse
note st = easeInOut 8 $ asNote harmonic st 1

main :: IO ()
main = do
  play 44000 song