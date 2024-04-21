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
  t <- time -< ()
  let n1 = sin (t * (2 * pi) * 336) * 3000
  let n2 = sin (t * (2 * pi) * 450) * 3500
  let n2 = sin (t * (2 * pi) * 150) * 3500
  let n = n1 + n2
  returnA -< SoundOutput { _soundOutL = floor n, _soundOutR = floor n }