module LambdaSound.Plot (plot, plotPart) where

import Data.Massiv.Array qualified as M
import Data.Text (Text, append, pack)
import Data.Text.IO qualified as T
import LambdaSound.Sound
import LambdaSound.Sampling (sampleSound)
import Data.Coerce (coerce)
import System.Console.ANSI

-- | Plots a sound in the terminal
plot :: Sound T Pulse -> IO ()
plot sound = plotPart (0, getDuration sound) sound

-- | Plots part of a sound in the terminal
plotPart :: (Duration, Duration) -> Sound T Pulse -> IO ()
plotPart (lD, rD) sound = do 
  cols <- maybe 80 snd <$> getTerminalSize
  let hz = coerce $ fromIntegral cols * (1 / (rD - lD))
      soundPart = takeSound (rD - lD) $ dropSound lD sound
  txt <- tabulateSamples 10 <$> sampleSound hz soundPart
  T.putStrLn txt

tabulateSamples :: Int -> M.Vector M.S Pulse -> Text
tabulateSamples rows samples =
  let maxSample = M.maximum' samples
      minSample = M.minimum' samples
      preparedSamples = M.compute $ M.map (\s -> (s - minSample) / (maxSample - minSample) * fromIntegral rows) samples
   in foldMap (drawRow preparedSamples) [0 .. rows]
  where
    drawRow :: M.Vector M.S Pulse -> Int -> Text
    drawRow preparedSamples r =
      append (pack "\n") $
        pack $
          M.toList $
            M.map
              ( \p ->
                  let x = p - fromIntegral r
                   in if x >= 0 && x < 1 
                        then
                          if x < (1 / 3)
                            then topDot
                            else
                              if x < (2 / 3)
                                then middleDot
                                else bottomDot
                        else ' '
              )
              preparedSamples
    topDot = '˙'
    middleDot = '·'
    bottomDot = '.'
