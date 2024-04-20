{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Sound ( ) where
import Control.Lens.TH
import Control.Lens
import FRP.Yampa

data SoundInput = SoundInput { _command :: Int }
$(makeLenses ''SoundInput)

data SoundOutput = SoundOutput { _sound :: [Int] }
$(makeLenses ''SoundOutput)

soundSystem :: SF SoundInput SoundOutput
soundSystem = proc input -> do
  returnA -< SoundOutput { _sound = [] }