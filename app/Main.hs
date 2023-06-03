module Main where

import LambdaSound

song :: Sound Pulse
song = parallel2 background melody

background :: Sound Pulse
background =
  repeatSound 4 $
    setDuration 2 $
      mconcat
        [ parallel $ note <$> [c3],
          repeatSound 3 $ parallel $ note <$> [e3, g3]
        ]

{-# ANN melody Cache #-}
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