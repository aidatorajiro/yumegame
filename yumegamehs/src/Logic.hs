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
      mySF,
      scriptReturns,
      sdlEvents,
      incomingMessage,
      script,
      pingMessage,
      timestamp,
      debugPrints,
      parseMessage) where

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
import Todo
import qualified Data.Text.Encoding as T
import SDL (EventPayload)
import Data.Binary (decode)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString (fromStrict)
import GHC.Int (Int64)

data WorldState = WorldState { _zoomFitDisabled :: Bool }
$(makeLenses ''WorldState)
initialWorldState :: WorldState
initialWorldState = WorldState { _zoomFitDisabled = False }

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
bpy.data.objects['#text.tooltip'].scale = mathutils.Vector((0.03, 0.03, 0.03))
|] <> "change_text(bpy.data.objects['#text.tooltip'] , '''" <> T.encodeUtf8 todo <> "''')"

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

getBtnEv :: Int32 -> Word8 -> [EventPayload] -> Event Int
getBtnEv a b sdlEvs = case mapMaybe (getJoyBtnValueFor a b) sdlEvs of
                  [] -> NoEvent
                  y:ys -> Event y

-- sendRecv :: [S.ByteString] -> 

parseMessage :: S.ByteString -> [S.ByteString]
parseMessage m =
  let n = (decode . fromStrict $ S.take 8 m) :: Int64
      m' = S.drop 8 m
      m'' = S.take (fromIntegral n) m'
      m''' = S.drop (fromIntegral n) m''
  in if S.null m then [] else m' : parseMessage m'''

-- | Main signal function with state
myStateSF :: SF (Outerworld, WorldState) (Innerworld, WorldState)
myStateSF = proc (outerworld, worldstate) -> do
  -- fetch outer world values
  t <- time -< ()
  let sdlEvs = SDL.eventPayload <$> (outerworld ^. sdlEvents)
  let incoming = parseMessage $ S.concat (outerworld ^. incomingMessage)
  let move_coeff = 4

  -- btn process
  let btn_y = getBtnEv 0 3 sdlEvs
  let btn_x = getBtnEv 0 2 sdlEvs
  let btn_b = getBtnEv 0 1 sdlEvs

  let state_zoomfit = if btn_b == Event 0 then not (worldstate ^. zoomFitDisabled) else worldstate ^. zoomFitDisabled

  let py_torch = if btn_y == Event 0 then Event "place_torch_around();" else NoEvent

  -- joy axis 0 (move)
  moveaxis0 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (fst movaxisPreset)) sdlEvs
  moveaxis1 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (snd movaxisPreset)) sdlEvs

  let movaxis = pairAbsThreshold moveaxis0 moveaxis1 commonThreshold

  let py_move_view = fromString . (\(d0, d1) ->
        "move_view(" <> show (fromIntegral d0 / 7000000 * move_coeff :: Double) <> ", 0, " <> show (fromIntegral d1 / 7000000 * move_coeff:: Double) <> ");") <$> movaxis :: Event S.ByteString

  hataxis0 <- lastOfListWithInit 0 -< mapMaybe (getJoyHatValueFor 0 0 SDL.HatDown SDL.HatCentered SDL.HatUp) sdlEvs

  let py_move_view_z
        | hataxis0 == 0 = NoEvent
        | otherwise = Event $ fromString $ "move_view(0, " <> show (fromIntegral hataxis0 / 1200 * move_coeff :: Double) <> ", 0);"

  -- joy axis 0 (rotate)
  rotaxis0 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (fst rotaxisXYPreset)) sdlEvs
  rotaxis1 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (snd rotaxisXYPreset)) sdlEvs

  rotaxis2 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (fst rotaxisZPreset)) sdlEvs
  rotaxis3 <- lastOfList -< mapMaybe (getJoyAxisValueFor 0 (snd rotaxisZPreset)) sdlEvs

  rotaxis_z <- convertHalfAxis -< (rotaxis2, rotaxis3)
  -- let rotaxis_z_noconv = bimap fromIntegral fromIntegral <$> pairAbsThreshold rotaxis2 rotaxis3 commonThreshold

  let rotaxis_xy = pairAbsThreshold rotaxis0 rotaxis1 commonThreshold

  let py_rotate_view = fromString . (\(d0, d1) ->
        "rotate_view(" <> show (fromIntegral d1 / (-10000000) :: Double) <> ", " <> show (fromIntegral d0 / (-15000000) :: Double) <> ", 0);") <$> rotaxis_xy

  let py_rotate_view_z = fromString . (\(d0, d1) ->
        "rotate_view(0, 0, " <> show (fromIntegral (d1 - d0) / 10000000 :: Double) <> ");") <$> rotaxis_z

  py_reset_1sec <- repeatedly 1 "reset_distance_of_view();" -< ()
  let py_reset_1sec' = if worldstate ^. zoomFitDisabled then NoEvent else py_reset_1sec

  let py_tooltip = "align_to_camera(bpy.data.objects['#text.tooltip'], RELLOC_BOTTOM_LEFT);" <$ btn_x

  py_save <- repeatedly 900 ("save_blend();" :: String) -< ()

  repeat_tenki <- (count :: SF (Event ()) (Event Int)) <<< repeatedly 30 () -< ()
  let tenki_table = (\x -> x ++ reverse x) [1.0 :: Double, 0.8, 0.75, 0.7, 0.5, 0.48, 0.43, 0.3, 0.2, 0.11, 0.1, 0.09, 0.04, 0.03, 0.02, 0.01, 0]
  let time_tenki = (\x -> tenki_table !! (x `mod` length tenki_table)) <$> repeat_tenki
  let py_tenki = (\x -> fromString $ "set_bg_strength(" <> show x <> ");") <$> time_tenki

  let py_debug = Event ""

  let py_move_events = S.concat <$> catEvents [py_move_view, py_rotate_view, py_rotate_view_z, py_move_view_z]

  py_send <- repeatedly 1 "sock_send(json.dumps({'type': 'mytype', 'data': 12345}))" -< ()

  -- output results
  py_reload <- now reloadScript -< ()
  let scr = catEvents [py_send, py_tooltip, py_move_events, py_tenki, py_debug, py_torch, py_reload, py_reset_1sec']
  returnA -< (Innerworld {
    _script = case scr of
      NoEvent -> []
      Event xs -> xs,
    _timestamp = t,
    _pingMessage = ["p"],
    _debugPrints = [] }, WorldState {_zoomFitDisabled = state_zoomfit})

-- | Main logic of the game
mySF :: SF Outerworld Innerworld
mySF = proc outerworld -> do
  a <- loopPre initialWorldState myStateSF -< outerworld
  returnA -< a