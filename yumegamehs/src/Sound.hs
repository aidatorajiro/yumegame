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

sinWave :: Double -> Double -> Time -> Double
sinWave f a t =  sin (t * (2 * pi) * f) * a

combinedSinWave :: [(Double, Double)] -> Time -> Double
combinedSinWave arry t = sum ((\x -> uncurry sinWave x t) <$> arry)

delayImpulse :: Time -> SF () Double
delayImpulse d = proc _ -> do
  t <- time -< ()
  td <- delay d 0 -< t
  returnA -< (if t > d + pi then 0 else (sin td ** 2) :: Double)

soundSystem :: SF SoundInput SoundOutput
soundSystem = proc input -> do
  t <- time -< ()
  imp1 <- delayImpulse 3 -< ()
  let n1 = combinedSinWave [(336, 3000), (450, 3500), (150, 3500)] t * imp1
  imp2 <- delayImpulse 5 -< ()
  let n2 = combinedSinWave [(510, 2000), (250, 4200), (400, 3000)] t * imp2
  returnA -< SoundOutput { _soundOutL = floor n1, _soundOutR = floor n2 }