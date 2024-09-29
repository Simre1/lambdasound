module LambdaSound.Music where

import LambdaSound.Sound

newtype Duration = Duration Float deriving (Eq, Show, Ord, Num, Fractional)

data Music = Music {
  duration :: Duration, 
  sound :: Sound 
}

modifySound :: (Sound -> Sound) -> Music -> Music
modifySound f (Music d s) = Music d (f s)

setDuration :: Duration -> Sound -> Music
setDuration = Music

(|->) :: Duration -> Sound -> Music
(|->) = setDuration
