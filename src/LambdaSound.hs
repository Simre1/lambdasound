-- |
-- Library users should implement this module ( @import LambdaSound@ ).
--
-- This module packages all the functions from the other modules and reexports them.
-- A good starting point to explore the documentation is the @LambdaSound.Sound@ module which
-- exports all the datatypes and many of the useful combinators you will use.
-- 
-- The @LambdaSound.Sample@ module contains some simple sound samples which you can play with the @LambdaSound.Play@ module. 
module LambdaSound
  ( -- * Sounds
    module Sound,

    -- * Create sounds
    module Create,

    -- * Play sounds
    module Play,

    -- * Notes
    module Note,

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
import LambdaSound.Convolution as Convolution
import LambdaSound.Create as Create
import LambdaSound.Effect as Effect
import LambdaSound.Filter as Filter
import LambdaSound.Note as Note
import LambdaSound.Play as Play
import LambdaSound.Plot as Plot
import LambdaSound.Samples as Sample
import LambdaSound.Sampling as Sampling
import LambdaSound.Sound as Sound
