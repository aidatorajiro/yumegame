{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import qualified Data.String as S
import Data.Int (Int32, Int16)
import Data.String (IsString(fromString))
import Data.String.QQ

data Outerworld = Outerworld { _scriptReturns :: [(Int, S.ByteString)], _sdlEvents :: [SDL.Event] }
$(makeLenses ''Outerworld)

initialOuterworld :: Outerworld
initialOuterworld = Outerworld { _scriptReturns = [], _sdlEvents = [] }

data Innerworld = Innerworld { _script :: [S.ByteString], _timestamp :: Double, _pingMessage :: [S.ByteString] }
$(makeLenses ''Innerworld)

lastOfList :: SF [x] (Event x)
lastOfList = proc xs -> do
  hold NoEvent -< (if null xs then NoEvent else Event (Event (last xs)))

getJoyAxisValueFor :: Int32 -> Word8 -> SDL.EventPayload -> Maybe Int16
getJoyAxisValueFor which axis ev = case ev of
  SDL.JoyAxisEvent x@(SDL.JoyAxisEventData w a v) -> if (w == which) && (a == axis) then Just v else Nothing
  _ -> Nothing

generateMoveViewScript :: SDL.JoyAxisEventData -> S.ByteString
generateMoveViewScript (SDL.JoyAxisEventData _ w v) = ""

absThreshold :: (Num a, Ord a) => a -> a -> a
absThreshold a b = if -a < b && b < a then 0 else b

reloadScript :: S.ByteString
reloadScript = [s|
print('Reloading hsfunctions...')
with open(os.path.join(proj_path, "hs", "yumegame", "hsfunctions.py")) as f:
  exec(f.read(), globals())
reset_distance_of_view()
|]

pairAbsThreshold :: (Num a, Ord a) => Event a -> Event a -> a -> Event (a, a)
pairAbsThreshold ev0 ev1 threshold = filterE (\(a, b) -> not (a == 0 && b == 0))
        (joinE (absThreshold threshold <$> ev0) (absThreshold threshold <$> ev1))

yaruzoo :: SF Outerworld Innerworld
yaruzoo = proc x -> do
  t <- time -< ()
  let sdlEvs = SDL.eventPayload <$> (x ^. sdlEvents)

  axis00 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 0) sdlEvs
  axis01 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 1) sdlEvs

  axis10 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 3) sdlEvs
  axis11 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 4) sdlEvs

  -- joy axis 0 (move)
  let axis0 = pairAbsThreshold axis00 axis01 1000

  let py_move_view = fromString . (\(d0, d1) -> 
        "move_view(" <> show (fromIntegral d0 / 15000000 :: Double) <> ", 0, " <> show (fromIntegral d1 / 15000000 :: Double) <> ")") <$> axis0
  
  let axis1 = pairAbsThreshold axis10 axis11 1000

  let py_rotate_view = fromString . (\(d0, d1) -> 
        "rotate_view(" <> show (fromIntegral d1 / (-15000000) :: Double) <> ", " <> show (fromIntegral d0 / (-15000000) :: Double) <> ", 0)") <$> axis1
  
  -- joy axis 0 (rotate)

  -- let debug = Event $ "print('''" <> fromString (show sdlEvs) <> "''')"

  -- output results
  py_reload <- now reloadScript -< ()
  let scr = catEvents [py_reload, py_move_view, py_rotate_view]
  ping <- repeatedly 1 () -< ()
  returnA -< Innerworld {
    _script = case scr of
      NoEvent -> []
      Event xs -> xs,
    _timestamp = t,
    _pingMessage = case ping of
      NoEvent -> []
      Event () -> ["p"] }