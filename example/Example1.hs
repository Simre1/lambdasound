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
