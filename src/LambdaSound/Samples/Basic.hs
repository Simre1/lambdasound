module LambdaSound.Samples.Basic where

import LambdaSound.Sound

sineWave :: Float -> Sound
sineWave hz = ElementarySound $ SineWave {amplitude = 1, hz = hz}
