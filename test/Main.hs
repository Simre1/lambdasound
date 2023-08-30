module Main (main) where

import Control.Monad (join, unless)
import Data.List.NonEmpty
import LambdaSound
import Test.Falsify.Generator qualified as Gen
import Test.Tasty
import Test.Tasty.Falsify
import  Data.Massiv.Array qualified as M
import GHC.Stack (HasCallStack)

main :: IO ()
main =
  defaultMain $
    testGroup
      "LambdaSound tests"
      [ testProperty "reverse" reverseProperty,
        testProperty "associative sequence" associativeSequence,
        testProperty "associative parallel" associativeParallel,
        testProperty "distributivity of parallel and sequence" distributivityParallelSequence,
        testProperty "takeSound" takeSoundProperty,
        testProperty "dropSound" dropSoundProperty,
        testProperty "take/drop" dropTakeSoundDuality
      ]

genSound :: Gen (Sound T Pulse)
genSound = do
  let basicSound =
        Gen.elem $
          setDuration 1 (constant 1)
            :| [ setDuration 1 (pulse 440),
                 setDuration 1 (harmonic 100),
                 setDuration 1 (noise 42)
               ]
  join $
    Gen.elem $
      ((<>) <$> basicSound <*> basicSound)
        :| [ (>>>) <$> basicSound <*> basicSound,
             takeSound 0.5 <$> basicSound,
             dropSound 0.3 <$> basicSound,
             changeTempo (** 1.2) <$> basicSound,
             amplify 2 <$> basicSound,
             reduce 2 <$> basicSound,
             raise 3 <$> basicSound
           ]

reverseProperty :: Property ()
reverseProperty = do
  sound <- genWith (\_ -> Just "Sound") genSound
  if reverseSound (reverseSound sound)
    `eqSound` sound
    then pure ()
    else testFailed "reverseSound failing"

associativeSequence :: Property ()
associativeSequence = do
  sound1 <- gen genSound
  sound2 <- gen genSound
  sound3 <- gen genSound
  unless ((sound1 >>> (sound2 >>> sound3)) `eqSound` ((sound1 >>> sound2) >>> sound3)) $
    testFailed "sequence not associative"

associativeParallel :: Property ()
associativeParallel = do
  sound1 <- gen genSound
  sound2 <- gen genSound
  sound3 <- gen genSound
  unless ((sound1 <> (sound2 <> sound3)) `almostEqSound` ((sound1 <> sound2) <> sound3)) $
    testFailed "parallel not associative"

distributivityParallelSequence :: Property ()
distributivityParallelSequence = do
  sound1 <- setDuration 1 <$> gen genSound
  sound2 <- setDuration 1 <$> gen genSound
  sound3 <- setDuration 1 <$> gen genSound
  sound4 <- setDuration 1 <$> gen genSound
  unless
    ( ((sound1 >>> sound2) <> (sound3 >>> sound4))
        `almostEqSound` ((sound1 <> sound3) >>> (sound2 <> sound4))
    )
    $ testFailed "parallel not associative"

takeSoundProperty :: Property ()
takeSoundProperty = do
  sound1 <- setDuration 1 <$> gen genSound
  sound2 <- setDuration 1 <$> gen genSound
  unless (takeSound 1 (sound1 >>> sound2) `eqSound` sound1) $
    testFailed "takeSound failed"

dropSoundProperty :: Property ()
dropSoundProperty = do
  sound1 <- setDuration 1 <$> gen genSound
  sound2 <- setDuration 1 <$> gen genSound
  unless (dropSound 1 (sound1 >>> sound2) `eqSound` sound2) $
    testFailed "dropSound failed"
    
dropTakeSoundDuality :: Property ()
dropTakeSoundDuality = do
  sound1 <- setDuration 1 <$> gen genSound
  sound2 <- setDuration 1 <$> gen genSound
  unless
    ( dropSound 1 (sound1 >>> sound2)
        `eqSound` reverseSound (takeSound 1 (reverseSound $ sound1 >>> sound2))
    )
    $ testFailed "drop/take duality failed"

eqSound :: Sound T Pulse -> Sound T Pulse -> Bool
eqSound s1 s2 = sampleSound (Hz 100) s1 == sampleSound (Hz 100) s2

almostEqSound :: Sound T Pulse -> Sound T Pulse -> Bool
almostEqSound s1 s2 =
  let x = sampleSound (Hz 100) s1
      y = sampleSound (Hz 100) s2
   in M.all (\a -> abs a < epsilon) $ M.zipWith (-) x y
  where
    epsilon = 5e-6
