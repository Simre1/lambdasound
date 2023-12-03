# Revision history for lambdasound

## 1.2.0 -- 2023-12-03

* Switch to proteaaudio version which uses RTAudio
* Update to GHC 9.6

## 1.1.0 -- 2023-10-16

* Add an example to showcase `loadSound`
* Support loading of wav and raw files as `Sound`
* Fix pathological behavior for repeated cached sounds
* Add `embedIO` to embed IO into sound generation

## 1.0.1 -- 2023-10-13

* Add `withSampledSound` and `withSampledSoundPulse`
* Fix incorrect memoization for sequential and parallel sounds

## 1.0.0 -- 2023-10-12

* First version. Released on an unsuspecting world.
