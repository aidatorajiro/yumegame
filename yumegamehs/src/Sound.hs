{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Sound ( SoundInput, SoundCommand, SoundOutput, soundCommandIn, soundOutL, soundOutR, initialSoundInput, soundSystem ) where
import Control.Lens.TH
import Control.Lens
import FRP.Yampa
import System.Random (mkStdGen)

initialSoundInput :: SoundInput
initialSoundInput = SoundInput { _soundCommandIn = NoEvent }

data SoundCommand = SoundCommand { _myInt :: Int }

data SoundInput = SoundInput { _soundCommandIn :: Event SoundCommand }
$(makeLenses ''SoundInput)

data SoundOutput = SoundOutput { _soundOutL :: Int, _soundOutR :: Int }
$(makeLenses ''SoundOutput)

noiseTest :: SF () Double
noiseTest = noise (mkStdGen 21722)

soundSystem :: SF SoundInput SoundOutput
soundSystem = proc input -> do
  n <- noiseTest -< ()
  let n' = (n - 0.5) * 2000
  returnA -< SoundOutput { _soundOutL = floor n', _soundOutR = floor n' }