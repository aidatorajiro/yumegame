{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Logic (Outerworld(Outerworld), initialOuterworld, Innerworld(Innerworld), yaruzoo, scriptReturns, sdlEvents, script, timestamp) where
import qualified SDL
import qualified Data.ByteString as S
import qualified Data.String as S
import FRP.Yampa

data Outerworld = Outerworld { scriptReturns :: [(Int, S.ByteString)], sdlEvents :: [SDL.Event] }
initialOuterworld :: Outerworld
initialOuterworld = Outerworld { scriptReturns = [], sdlEvents = [] }

data Innerworld = Innerworld { script :: [S.ByteString], timestamp :: Double }

yaruzoo :: SF Outerworld Innerworld
yaruzoo = proc x -> do
  let sdlEvs = sdlEvents x
  lastControllerEv <- hold NoEvent -< (if null sdlEvs then NoEvent else Event (Event (last sdlEvs)))
  ev <- repeatedly 1 () -< ()
  let controllerEvPerSec = ev >> lastControllerEv
  t <- time -< ()
  let scr = case ev of 
        Event _ -> ["print('" <> S.fromString (show controllerEvPerSec) <> "')"]
        NoEvent -> []
  returnA -< Innerworld { script = scr, timestamp = 0 }