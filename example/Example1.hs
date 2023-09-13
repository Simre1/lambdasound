import Data.Coerce (coerce)
import LambdaSound

main :: IO ()
main = play 44100 0.4 $
    let downwards = sequentially $ setDuration 0.25 . note
          <$> [g4, f4, e4, d4, f4, e4, d4, c4, e4, d4, c4, b3]
     in sequentially
          [ downwards,
            dropSound 0.25 (reverseSound $ dropSound 0.5 downwards),
            setDuration 0.25 (parallel $ note <$> [c4, e4]),
            setDuration 0.25 (parallel $ note <$> [c4, e4, g4]),
            setDuration 0.5 (parallel $ note <$> [c4, g4, c5])
          ]

-- Design a note sample
note :: Semitone -> Sound I Pulse
note st = easeInOut 2 $ asNote sawWave st + asNote (harmonic triangleWave) st

-- -- simpleReverb 0.1 $ applyIIRFilter (highPassFilter 600 1) sound

-- sound :: Sound T Pulse
-- sound = melody <> background

-- background :: Sound T Pulse
-- background =
--   repeatSound 3 $
--     sequentially $
--       fmap
--         (setDuration 0.5)
--         [ note c3,
--           parallel $ note <$> [e3, g3],
--           parallel $ note <$> [e3, g3],
--           parallel $ note <$> [e3, g3]
--         ]

-- melody :: Sound T Pulse
-- melody =
--   let mel =
--         repeatSound
--           3
--           ( sequentially $
--               fmap
--                 (setDuration 0.5)
--                 [ note c4,
--                   note e4,
--                   note g4,
--                   note e4
--                 ]
--           )
--           >>> end
--       end = setDuration 2 $ parallel [note c4, note c3, note g3]
--    in mel

-- note :: Semitone -> Sound T Pulse
-- note st =
--   applyEnvelope (Envelope 0.2 0.1 0.2 0.8) $
--     setDuration 1 $
--       asNote (harmonic sineWave) st

-- -- Further examples

-- metronome :: Sound T Pulse
-- metronome = repeatSound 10 $ setDuration 1 $ note c4 >>> setDuration 2 silence

-- upSound :: Sound T Pulse
-- upSound =
--   zipSoundWith (*) ((\p -> 1 - coerce p) <$> progress) $
--     speedUp $
--       upwards >>> takeSound 2 (raiseSemitones 12 upwards) >>> setDuration 1 (note g5)

-- upwards :: Sound T Pulse
-- upwards = setDuration 3.5 $ sequentially $ note <$> [c4, d4, e4, f4, g4, a4, b4]

-- speedUp :: Sound T Pulse -> Sound T Pulse
-- speedUp = changeTempo $ \p -> p ** 2
