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
      timestamp,
      debugPrints) where

import qualified SDL
import qualified Data.ByteString as S
import FRP.Yampa
import Control.Lens.TH
import Control.Lens
import Data.Maybe (mapMaybe, isJust)
import Data.Word (Word8)
import Data.Int (Int32, Int16)
import Data.String (IsString(fromString))
import Data.String.QQ
import GHC.Num (Natural(NB))
import qualified System.Info as SI
import qualified Data.String as S

data Outerworld = Outerworld { _scriptReturns :: [(Int, S.ByteString)], _sdlEvents :: [SDL.Event], _incomingMessage :: [S.ByteString] }
$(makeLenses ''Outerworld)

initialOuterworld :: Outerworld
initialOuterworld = Outerworld { _scriptReturns = [], _sdlEvents = [], _incomingMessage = [] }

data Innerworld = Innerworld { _script :: [S.ByteString], _timestamp :: Double, _pingMessage :: [S.ByteString], _debugPrints :: [String] }
$(makeLenses ''Innerworld)

-- | Obtains the last element from the data flow of list. If there is no such value, signals `NoEvent`.
lastOfList :: SF [x] (Event x)
lastOfList = proc xs -> do
  hold NoEvent -< (if null xs then NoEvent else Event (Event (last xs)))

-- | Obtains the last element from the data flow of list. If there is no such value, signals given initial value `i`.
lastOfListWithInit :: x -> SF [x] x
lastOfListWithInit i = proc xs -> do
  hold i -< (if null xs then NoEvent else Event (last xs))

-- | obtains axis value for specific combination of controller id (`which`) and axis id (`axis`) from given SDL event `ev`.
getJoyAxisValueFor :: Int32 -> Word8 -> SDL.EventPayload -> Maybe Int16
getJoyAxisValueFor which axis ev = case ev of
  SDL.JoyAxisEvent x@(SDL.JoyAxisEventData w a v) -> if (w == which) && (a == axis) then Just v else Nothing
  _ -> Nothing

-- | obtains button value for specific combination of controller id (`controllerID`) and button id (`buttonID`) from given SDL event `ev`.
getJoyBtnValueFor :: Int32 -> Word8 -> SDL.EventPayload -> Maybe Int
getJoyBtnValueFor controllerID buttonID ev = case ev of
    SDL.JoyButtonEvent x@(SDL.JoyButtonEventData c b state) -> if b == buttonID then
        case state of
            SDL.JoyButtonPressed -> Just 1
            SDL.JoyButtonReleased -> Just 0
      else Nothing
    _ -> Nothing

-- | obtains hat value for specific combination of controller id (`controllerID`) and hat id (`hatID`) from given SDL event `ev`.
getJoyHatValueFor :: Int32 -> Word8 -> SDL.JoyHatPosition -> SDL.JoyHatPosition -> SDL.JoyHatPosition -> SDL.EventPayload -> Maybe Int
getJoyHatValueFor controllerID hatID posForMinusOne posForZero posForOne ev = case ev of
  SDL.JoyHatEvent x@(SDL.JoyHatEventData c h p) ->
    let val
          | p == posForMinusOne = Just (-1)
          | p == posForZero = Just 0
          | p == posForOne = Just 1
          | otherwise = Nothing
    in if (c == controllerID) && (h == hatID) then val else Nothing
  _ -> Nothing

-- | Returns 0 if the absolute value of `b` is smaller than `a`. Returns `b` otherwise.
absThreshold :: (Num a, Ord a) => a -> a -> a
absThreshold a b = if -a < b && b < a then 0 else b

-- | Initialization script in python
reloadScript :: S.ByteString
reloadScript = [s|
print('Reloading hsfunctions...')
with open(os.path.join(proj_path, "yumegamehs", "hsfunctions.py")) as f:
  exec(f.read(), globals())
reset_distance_of_view()
|]

-- | Emit an `Event` if the absolute value of the given two `Event` are both greater than given threshold. NoEvent will be treated as zero value.
pairAbsThreshold' :: (Num a, Ord a) => Event a -> Event a -> a -> Event (a, a)
pairAbsThreshold' (Event x) (Event y) threshold = pairAbsThreshold (Event x) (Event y) threshold
pairAbsThreshold' (Event x) NoEvent threshold = pairAbsThreshold (Event x) (Event 0) threshold
pairAbsThreshold' NoEvent (Event y) threshold = pairAbsThreshold (Event 0) (Event y) threshold
pairAbsThreshold' _ _ _ = NoEvent

-- | Emit an Event if the given two Event are both up and their absolute value are both greater than given threshold.
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

-- | common threshold value for controller axis
commonThreshold :: Int16
commonThreshold = 2000

-- | pick the first element of the list and emit it as an `Event`
pickList :: [a] -> Event a
pickList [] = NoEvent
pickList (x:xs) = Event x

convertHalfAxis :: SF (Event Int16, Event Int16) (Event (Int, Int))
convertHalfAxis = proc (rotaxis2, rotaxis3) -> do
  let axis_offset i = fromIntegral i + 32768 :: Int
  rotaxis2' <- dropUntil (<(-32768 + commonThreshold)) -< rotaxis2
  rotaxis3' <- dropUntil (<(-32768 + commonThreshold)) -< rotaxis3
  let rotaxis_z = pairAbsThreshold' (axis_offset <$> rotaxis2') (axis_offset <$> rotaxis3') (fromIntegral commonThreshold)
  returnA -< rotaxis_z

movaxisPreset :: (Word8, Word8)
movaxisPreset = if SI.os == "mingw32" then (0, 1) else (0, 1)

rotaxisXYPreset :: (Word8, Word8)
rotaxisXYPreset = if SI.os == "mingw32" then (2, 3) else (3, 4)

rotaxisZPreset :: (Word8, Word8)
rotaxisZPreset = if SI.os == "mingw32" then (5, 4) else (5, 2)

-- | Main logic of the game
yaruzoo :: SF Outerworld Innerworld
yaruzoo = proc x -> do
  -- fetch outer world values
  t <- time -< ()
  let sdlEvs = SDL.eventPayload <$> (x ^. sdlEvents)
  let incoming = x ^. incomingMessage
  let move_coeff = 4

  -- y btn process
  let btn_y = case mapMaybe (getJoyBtnValueFor 0 3) sdlEvs of
                  [] -> NoEvent
                  y:ys -> if y == 1 then Event y else NoEvent

  let py_torch = fmap (const "place_torch_around()") btn_y

  -- joy axis 0 (move)
  moveaxis0 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (fst movaxisPreset)) sdlEvs
  moveaxis1 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (snd movaxisPreset)) sdlEvs

  let movaxis = pairAbsThreshold moveaxis0 moveaxis1 commonThreshold

  let py_move_view = fromString . (\(d0, d1) ->
        "move_view(" <> show (fromIntegral d0 / 15000000 * move_coeff :: Double) <> ", 0, " <> show (fromIntegral d1 / 15000000 * move_coeff:: Double) <> ")") <$> movaxis

  hataxis0 <- lastOfListWithInit 0 -< mapMaybe (getJoyHatValueFor 0 0 SDL.HatDown SDL.HatCentered SDL.HatUp) sdlEvs

  let py_move_view_z
        | hataxis0 == 0 = NoEvent
        | otherwise = Event $ fromString $ "move_view(0, " <> show (fromIntegral hataxis0 / 2000 * move_coeff :: Double) <> ", 0)"

  -- joy axis 0 (rotate)
  rotaxis0 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (fst rotaxisXYPreset)) sdlEvs
  rotaxis1 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (snd rotaxisXYPreset)) sdlEvs

  rotaxis2 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (fst rotaxisZPreset)) sdlEvs
  rotaxis3 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (snd rotaxisZPreset)) sdlEvs

  rotaxis_z <- convertHalfAxis -< (rotaxis2, rotaxis3)
  -- let rotaxis_z_noconv = bimap fromIntegral fromIntegral <$> pairAbsThreshold rotaxis2 rotaxis3 commonThreshold

  let rotaxis_xy = pairAbsThreshold rotaxis0 rotaxis1 commonThreshold

  let py_rotate_view = fromString . (\(d0, d1) ->
        "rotate_view(" <> show (fromIntegral d1 / (-15000000) :: Double) <> ", " <> show (fromIntegral d0 / (-15000000) :: Double) <> ", 0)") <$> rotaxis_xy

  let py_rotate_view_z = fromString . (\(d0, d1) ->
        "rotate_view(0, 0, " <> show (fromIntegral (d1 - d0) / 15000000 :: Double) <> ")") <$> rotaxis_z

  --py_reset_1sec <- repeatedly 1 "reset_distance_of_view()" -< ()
  py_reset_1sec <- repeatedly 1 "()" -< ()

  py_save <- repeatedly 900 "save_blend()" -< ()

  -- let py_debug = if null sdlEvs then NoEvent else Event ("debugprint('''" <> S.fromString (show sdlEvs) <> "''')")
  let py_debug = NoEvent

  let debug = []

  -- output results
  py_reload <- now reloadScript -< ()
  let scr = catEvents [py_debug, py_torch, py_reload, py_move_view, py_rotate_view, py_rotate_view_z, py_reset_1sec, py_move_view_z]
  returnA -< Innerworld {
    _script = case scr of
      NoEvent -> []
      Event xs -> xs,
    _timestamp = t,
    _pingMessage = ["p"],
    _debugPrints = debug }
