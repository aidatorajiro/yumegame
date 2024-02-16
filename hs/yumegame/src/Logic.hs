{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Logic (
      Outerworld(Outerworld),
      initialOuterworld,
      Innerworld(Innerworld),
      yaruzoo,
      _scriptReturns,
      scriptReturns,
      _sdlEvents,
      sdlEvents,
      _script,
      script,
      _timestamp,
      timestamp) where
import qualified SDL
import qualified Data.ByteString as S
import qualified Data.String as S
import FRP.Yampa
import Control.Lens.TH
import Control.Lens

data Outerworld = Outerworld { _scriptReturns :: [(Int, S.ByteString)], _sdlEvents :: [SDL.Event] }
$(makeLenses ''Outerworld)

initialOuterworld :: Outerworld
initialOuterworld = Outerworld { _scriptReturns = [], _sdlEvents = [] }

data Innerworld = Innerworld { _script :: [S.ByteString], _timestamp :: Double }
$(makeLenses ''Innerworld)

lastOfList :: SF [x] (Event x)
lastOfList = proc xs -> do
      hold NoEvent -< (if null xs then NoEvent else Event (Event (last xs)))

isJoyAxisEvent :: SDL.Event -> Bool
isJoyAxisEvent e = case SDL.eventPayload e of
      SDL.JoyAxisEvent _ -> True
      _ -> False

yaruzoo :: SF Outerworld Innerworld
yaruzoo = proc x -> do
  let sdlEvs = x ^. sdlEvents
  lastControllerEv <- lastOfList -< filter isJoyAxisEvent sdlEvs
  everySecond <- repeatedly 1 () -< ()
  t <- time -< ()
  let scr = case everySecond of 
        Event _ -> ["print('" <> S.fromString (show lastControllerEv) <> "')"]
        NoEvent -> []
  returnA -< Innerworld { _script = scr, _timestamp = 0 }