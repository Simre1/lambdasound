import LambdaSound

main :: IO ()
main = do
  play 44100 1 $ setDuration (getDuration sound * 60 / 70) sound
  -- samples <- sampleSound 44100 $ setDuration (getDuration sound * 60 / 70) sound
  -- saveWav "sound.wav" 44100 samples

sound :: Sound T Pulse
sound =
  simpleReverb 0.15 $
    (melody1 >>> melody2 >>> melody3)
      <> reduce 1.3 (background1 >>> background2 >>> background3)

melody1 :: Sound T Pulse
melody1 =
  sequentially
    [ lEn $ melodyNote g4,
      lEn $ melodyNote f4,
      lEn $ 0.5 |-> melodyNote g4,
      lEn $ 0.5 |-> melodyNote e4,
      lEn $ melodyNote c4
    ]

melody2 :: Sound T Pulse
melody2 =
  sequentially
    [ lEn $ melodyNote g4,
      lEn $ melodyNote f4,
      lEn $ 0.5 |-> melodyNote g4,
      lEn $ 0.5 |-> melodyNote e4,
      lEn (0.5 |-> melodyNote c4)
        <> lEn (melodyNote g4)
        <> (0.5 |-> silence >>> lEn (0.5 |-> melodyNote c5))
    ]

melody3 :: Sound T Pulse
melody3 =
  parallel
    [ amplify 1.5 (pEn $ 2 |-> (melodyNote g4 <> melodyNote c5 <> melodyNote e5)),
      sequentially
        [ 1 |-> silence,
          lEn (0.5 |-> melodyNote g4),
          lEn (0.5 |-> melodyNote e4),
          lEn (0.5 |-> melodyNote g4),
          lEn (0.5 |-> melodyNote e4),
          lEn (1 |-> melodyNote c4) <> lEn (1 |-> melodyNote g3)
        ]
    ]

background1 :: Sound T Pulse
background1 =
  sequentially $
    fmap
      (lEn . setDuration 0.5 . backgroundNote)
      [c3, g3, c4, g3]
      ++ [ parallel $ lEn . backgroundNote <$> [g3, c4],
           lEn $ 0.5 |-> backgroundNote a3,
           lEn $ 0.5 |-> backgroundNote b3
         ]

background2 :: Sound T Pulse
background2 =
  sequentially $
    fmap
      (lEn . setDuration 0.5 . backgroundNote)
      [c3, g3, c4, g3]
      ++ [ parallel $ lEn . backgroundNote <$> [g3, c4],
           parallel $ lEn . backgroundNote <$> [c3, e3, g3]
         ]

background3 :: Sound T Pulse
background3 =
  sequentially $
    fmap
      (lEn . setDuration 0.5 . backgroundNote)
      [b2, d3, e3, g3, g3, e3]
      ++ [parallel [lEn $ backgroundNote c3]]


lEn :: Sound 'T Pulse -> Sound 'T Pulse
lEn = applyEnvelope (Envelope 0.1 0.3 0.2 0.4)

pEn :: Sound 'T Pulse -> Sound 'T Pulse
pEn = applyEnvelope (Envelope 0.08 0.8 0.4 0.2)

melodyNote :: Semitone -> Sound T Pulse
melodyNote st = setDuration 1 $ reduce 2 (asNote (harmonic sineWave) st) + asNote squareWave st

backgroundNote :: Semitone -> Sound T Pulse
backgroundNote st = setDuration 1 $ asNote triangleWave st + reduce 2 (asNote (harmonic sineWave) st)
