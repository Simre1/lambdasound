module Main (main) where

import Control.Monad (join, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty
import Data.Massiv.Array qualified as M
import LambdaSound
import Test.Falsify.Generator qualified as Gen
import Test.Tasty
import Test.Tasty.Falsify
import System.IO.Unsafe (unsafePerformIO)

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
        testProperty "take/drop" dropTakeSoundDuality,
        testProperty "cache does not change sound" cacheDoesNotChangeSound 
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
  assertEquality "reverseSound failing" $
    reverseSound (reverseSound sound)
      `eqSound` sound

associativeSequence :: Property ()
associativeSequence = do
  sound1 <- gen genSound
  sound2 <- gen genSound
  sound3 <- gen genSound
  assertEquality "sequence not associative" $ (sound1 >>> (sound2 >>> sound3)) `eqSound` ((sound1 >>> sound2) >>> sound3)

associativeParallel :: Property ()
associativeParallel = do
  sound1 <- gen genSound
  sound2 <- gen genSound
  sound3 <- gen genSound
  assertEquality "parallel not associative" $ (sound1 <> (sound2 <> sound3)) `almostEqSound` ((sound1 <> sound2) <> sound3)

distributivityParallelSequence :: Property ()
distributivityParallelSequence = do
  sound1 <- setDuration 1 <$> gen genSound
  sound2 <- setDuration 1 <$> gen genSound
  sound3 <- setDuration 1 <$> gen genSound
  sound4 <- setDuration 1 <$> gen genSound
  assertEquality "parallel not associative" $
    ((sound1 >>> sound2) <> (sound3 >>> sound4))
      `almostEqSound` ((sound1 <> sound3) >>> (sound2 <> sound4))

takeSoundProperty :: Property ()
takeSoundProperty = do
  sound1 <- setDuration 1 <$> gen genSound
  sound2 <- setDuration 1 <$> gen genSound
  assertEquality "takeSound failed" $ takeSound 1 (sound1 >>> sound2) `eqSound` sound1

dropSoundProperty :: Property ()
dropSoundProperty = do
  sound1 <- setDuration 1 <$> gen genSound
  sound2 <- setDuration 1 <$> gen genSound
  assertEquality "dropSound failed" $ dropSound 1 (sound1 >>> sound2) `eqSound` sound2

dropTakeSoundDuality :: Property ()
dropTakeSoundDuality = do
  sound1 <- setDuration 1 <$> gen genSound
  sound2 <- setDuration 1 <$> gen genSound
  assertEquality "drop/take duality failed" $
    dropSound 1 (sound1 >>> sound2)
      `eqSound` reverseSound (takeSound 1 (reverseSound $ sound1 >>> sound2))

cacheDoesNotChangeSound :: Property ()
cacheDoesNotChangeSound= do
  sound <- setDuration 1 <$> gen genSound
  assertEquality "cache changed sound" $
    cache sound
      `eqSound` sound

assertEquality :: String -> IO Bool -> Property ()
assertEquality failText check = do
  let areEqual = unsafePerformIO $ liftIO check
  unless areEqual (testFailed failText)

eqSound :: Sound T Pulse -> Sound T Pulse -> IO Bool
eqSound s1 s2 = (==) <$> sampleSound (Hz 100) s1 <*> sampleSound (Hz 100) s2

almostEqSound :: Sound T Pulse -> Sound T Pulse -> IO Bool
almostEqSound s1 s2 = do
  x <- sampleSound (Hz 100) s1
  y <- sampleSound (Hz 100) s2
  pure $ M.all (\a -> abs a < epsilon) $ M.zipWith (-) x y
  where
    epsilon = 5e-6
