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
      incomingMessage,
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

data Outerworld = Outerworld { _scriptReturns :: [(Int, S.ByteString)], _sdlEvents :: [SDL.Event], _incomingMessage :: [S.ByteString] }
$(makeLenses ''Outerworld)

initialOuterworld :: Outerworld
initialOuterworld = Outerworld { _scriptReturns = [], _sdlEvents = [], _incomingMessage = [] }

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

-- | Drop Event until the given condition is satisfied.
-- | Once the condition is satisfied, it will always deliver Event regardless of the condition afterwards.
dropUntil :: (a -> Bool) -> SF (Event a) (Event a)
dropUntil condition =
  let has_condition_already_met = sscan (\bool dat -> bool || case dat of 
                  Event dat_in -> condition dat_in
                  NoEvent -> False) False
  in proc x -> do
        b <- has_condition_already_met -< x
        returnA -< if b then x else NoEvent

yaruzoo :: SF Outerworld Innerworld
yaruzoo = proc x -> do
  -- fetch outer world values
  t <- time -< ()
  let sdlEvs = SDL.eventPayload <$> (x ^. sdlEvents)
  let incoming = x ^. incomingMessage

  -- joy axis 0 (move)
  moveaxis0 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 0) sdlEvs
  moveaxis1 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 1) sdlEvs

  let movaxis = pairAbsThreshold moveaxis0 moveaxis1 2000

  let py_move_view = fromString . (\(d0, d1) -> 
        "move_view(" <> show (fromIntegral d0 / 15000000 :: Double) <> ", 0, " <> show (fromIntegral d1 / 15000000 :: Double) <> ")") <$> movaxis

  -- joy axis 0 (rotate)
  rotaxis0 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 3) sdlEvs
  rotaxis1 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 4) sdlEvs
  
  rotaxis2 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 2) sdlEvs
  rotaxis3 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 5) sdlEvs

  rotaxis2' <- dropUntil (<(-32768+2000)) -< rotaxis2
  rotaxis3' <- dropUntil (<(-32768+2000)) -< rotaxis3
  
  let rotaxis_xy = pairAbsThreshold rotaxis0 rotaxis1 2000

  let py_rotate_view = fromString . (\(d0, d1) -> 
        "rotate_view(" <> show (fromIntegral d1 / (-15000000) :: Double) <> ", " <> show (fromIntegral d0 / (-15000000) :: Double) <> ", 0)") <$> rotaxis_xy
  
  let axis_offset i = fromIntegral i + 32768 :: Int
  let rotaxis_z = pairAbsThreshold (axis_offset <$> rotaxis2') (axis_offset <$> rotaxis3') 2000
  let py_rotate_view_z = fromString . (\(d0, d1) -> 
        "rotate_view(0, 0, " <> show (fromIntegral (d0 - d1) / 15000000 :: Double) <> ")") <$> rotaxis_z
  
  debug_sock_send <- repeatedly 1 "sock_send(b'12345')" -< ()

  -- output results
  py_reload <- now reloadScript -< ()
  let scr = catEvents [py_reload, py_move_view, py_rotate_view, py_rotate_view_z, debug_sock_send]
  ping <- repeatedly 1 () -< ()
  returnA -< Innerworld {
    _script = case scr of
      NoEvent -> []
      Event xs -> xs,
    _timestamp = t,
    _pingMessage = ["p"] }