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
      scriptReturns,
      sdlEvents,
      script,
      pingMessage,
      timestamp) where

import qualified SDL
import qualified Data.ByteString as S
import FRP.Yampa
import Control.Lens.TH
import Control.Lens

data Outerworld = Outerworld { _scriptReturns :: [(Int, S.ByteString)], _sdlEvents :: [SDL.Event] }
$(makeLenses ''Outerworld)

initialOuterworld :: Outerworld
initialOuterworld = Outerworld { _scriptReturns = [], _sdlEvents = [] }

data Innerworld = Innerworld { _script :: [S.ByteString], _timestamp :: Double, _pingMessage :: [S.ByteString] }
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
  t <- time -< ()
  let sdlEvs = x ^. sdlEvents
  lastControllerEv <- lastOfList -< filter isJoyAxisEvent sdlEvs
  py_reload <- now "print('Reloading hsfunctions module...')\nimport importlib\nimport hsfunctions\nimportlib.reload(hsfunctions)" -< ()
  let scr = catEvents [py_reload]
  ping <- repeatedly 1 () -< ()
  returnA -< Innerworld {
    _script = case scr of
      NoEvent -> []
      Event xs -> xs,
    _timestamp = t,
    _pingMessage = case ping of
      NoEvent -> []
      Event () -> ["p"] }