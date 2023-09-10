module Main where

import Data.Coerce (coerce)
import LambdaSound
import LambdaSound.Filter

main :: IO ()
main = play 44100 0.4 $ applyIIRFilter (highPassFilter 1000  1) $ song

song :: Sound T Pulse
song = melody <> reduce 2 background

background :: Sound T Pulse
background =  
    sequentially $
      mconcat $
        replicate 4 $
          fmap
            (setDuration 0.5)
            [ note c3,
              parallel $ note <$> [e3, g3],
              parallel $ note <$> [e3, g3],
              parallel $ note <$> [e3, g3]
            ]

melody :: Sound T Pulse
melody =
  let mel =
        
          sequentially $
            (++ [end]) $
              mconcat $
                replicate 4 $
                  fmap
                    (setDuration 0.5)
                    [ note c4,
                      note e4,
                      note g4,
                      note e4
                    ]
      end = setDuration 2 $ note c4
   in mel

note :: Semitone -> Sound T Pulse
note st = applyEnvelope (Envelope 0.2 0.1 0.2 0.8) $ setDuration 1 $ asNote (harmonic sineWave) st

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
