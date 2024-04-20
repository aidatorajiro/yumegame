{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Sound ( ) where
import Control.Lens.TH
import Control.Lens
import FRP.Yampa

data SoundState = SoundState { _myInt :: Int }

data SoundCommand = ExportState

data SoundInput = SoundInput { _initialState :: SoundState, _command :: SoundCommand }
$(makeLenses ''SoundInput)

data SoundOutput = SoundOutput { _stateOut :: Maybe SoundState, _sound :: Int }
$(makeLenses ''SoundOutput)

soundSystem :: SF SoundInput SoundOutput
soundSystem = proc input -> do
  returnA -< SoundOutput { _sound = 0, _stateOut = Nothing}