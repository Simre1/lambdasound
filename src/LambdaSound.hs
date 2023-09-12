-- |
-- Library users should implement this module (@import LambdaSound@).
--
-- This module packages all the functions from the other modules and reexports them.
-- A good starting place to explore the documentation is the *LambdaSound.Sound* module which
-- exports all the datatypes and many of the useful combinators you will use.
module LambdaSound
  ( -- * Sounds
    module Sound,

    -- * Notes
    module Note,

    -- * Play sounds
    module Play,

    -- * Effects
    module Effect,

    -- * Convolution
    module Convolution,
    -- * Sound samples
    module Sample,

    -- * Filter sounds
    module Filter,

    -- * Plot sounds
    module Plot,

    -- * Sample sounds,
    module Sampling,

    -- * Cache sounds
    module Cache,
  )
where

import LambdaSound.Cache as Cache
import LambdaSound.Effect as Effect
import LambdaSound.Note as Note
import LambdaSound.Play as Play
import LambdaSound.Plot as Plot
import LambdaSound.Samples as Sample
import LambdaSound.Sampling as Sampling
import LambdaSound.Sound as Sound
import LambdaSound.Convolution as Convolution
import LambdaSound.Filter as Filter
