{-# LANGUAGE OverloadedStrings #-}

module Ui.Utils (module Ui.Utils) where

import Reactive.Banana as R
import Reactive.Banana.Frameworks
import Data.Word
import Data.IORef
import SDL
import SDL.Font
import Data.Text (pack)
import Foreign.C.Types (CInt)

import Ui.Types
import Uno.Common.Types
import Utils


white, black, grey :: V4 Int
white = V4 255 255 255 255
black = V4 0 0 0 255
grey  = V4 125 125 125 255

red, green, blue, yellow :: V4 Int
red    = V4 255 0 0 255
green  = V4 0 255 0 255
blue   = V4 0 0 255 255
yellow = V4 255 255 0 255

fRed :: SDL.Font.Color
fRed = V4 255 0 0 255

fBlack :: SDL.Font.Color
fBlack = V4 0 0 0 255

showColor :: GColor -> String
showColor (V4 r g b a) = show r ++ show g ++ show b ++ show a

{-----------------------------------------------------------------------------
    Card tileset  
------------------------------------------------------------------------------}
tileWidth, tileHeight :: CInt
tileWidth = 450
tileHeight = 600

colorToKey :: CardColor -> CInt
colorToKey Blue = 0
colorToKey Green = 1
colorToKey Yellow = 2
colorToKey Red = 3
colorToKey Colorless = 4
colorToKey _ = undefined

makeColorMap :: CardColor -> [(Card, (CInt, CInt))]
makeColorMap col = 
  let colKey = colorToKey col in
  zip ([Card (Number i, col) | i <- [0..9]] ++ [Card (Skip, col), Card (Add 2, col),  Card (Switch, col)]) $
  zip [0..] [colKey | _ <- [1..13]] 

colorTextureTileMap :: [(Card, (CInt, CInt))]
colorTextureTileMap = 
  let colorMaps = concatMap makeColorMap [Red, Green, Yellow, Blue]
  in colorMaps ++ 
    [ (Card (AddColorless (4, Null), Colorless), (0, 4))
    , (Card (ChangeColor Null, Colorless), (1, 4))
    , (Card (Blank, Colorless), (2, 4))
    ]

getTileRect :: (CInt, CInt) -> SDL.Rectangle CInt
getTileRect (x, y) = Rectangle 
          (P (V2 (x * tileWidth) (y * tileHeight)))
          (V2 tileWidth tileHeight)

cardToTile :: Card -> SDL.Rectangle CInt
cardToTile card = case lookup card colorTextureTileMap of
  Just pair -> getTileRect pair
  Nothing -> undefined

toggleCardChoice :: R.Event AppEvent -> R.Event ([Int] -> [Int])
toggleCardChoice = fmap makexor . filterE isToggleCardChoiceEvent
  where
    makexor (ToggleCardChoice n) = xorSet (Just n)
    makexor _ = id

{-----------------------------------------------------------------------------
    SDL Keyboard logic
------------------------------------------------------------------------------}

-- Check if an event is a key press
isKeyPressSDLEvent :: SDL.Event -> Bool
isKeyPressSDLEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent kbEvent ->
      SDL.keyboardEventKeyMotion kbEvent == SDL.Pressed
    _ -> False

isMousePressedSDLEvent :: SDL.Event -> Bool
isMousePressedSDLEvent event = 
  case SDL.eventPayload event of
    SDL.MouseButtonEvent emouse ->
      SDL.mouseButtonEventMotion emouse == SDL.Pressed
    _ -> False

collectMousePressedEvents :: [SDL.Event] -> [SDL.MouseButtonEventData]
collectMousePressedEvents xs = aux xs []
  where 
    aux [] acc = acc
    aux (e:es) acc = 
      case SDL.eventPayload e of
        SDL.MouseButtonEvent emouse -> 
          if SDL.mouseButtonEventMotion emouse == SDL.Pressed 
            then aux es $ emouse : acc
            else aux es acc
        _ -> aux es acc

-- Check if an event is an exit event (Q pressed)
eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
    _ -> False
  
-- Get the key code from a key press event
getKeyCode :: SDL.Event -> Maybe SDL.Keycode
getKeyCode event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      Just (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent))
    _ -> Nothing


{-----------------------------------------------------------------------------
    UI  
------------------------------------------------------------------------------}

class WidgetInteraction a where
  isWidgetHovered :: a -> V2 Int -> Bool 
  isWidgetClicked :: a -> SDL.EventPayload -> Bool
  updateColor     :: a -> GColor -> IO ()
  updateSelection :: a -> Bool -> IO ()

instance WidgetInteraction Button where
  isWidgetHovered (Button _ _ (V2 x y) (V2 w h) _ _) mpos = isPointInRect mpos (x, y, w, h)

  isWidgetClicked button (MouseButtonEvent mouseEv) =
    SDL.mouseButtonEventMotion mouseEv == Pressed &&
    isWidgetHovered button (mousePos mouseEv) 
  isWidgetClicked _ _ = False

  updateColor (Button _ _ _ _ colorRef _) newColor = do
    writeIORef colorRef newColor 

  updateSelection btn isSelected = do
    writeIORef (buttonSelected btn) isSelected 

instance WidgetInteraction ImageButton where
  isWidgetHovered (ImageButton _ _ (V2 x y) (V2 w h) _ _) mpos = 
    case map fromIntegral [x, y, w, h] of
      [x', y', w', h'] -> isPointInRect mpos (x', y', w', h')
      _ -> False

  isWidgetClicked imgButton (MouseButtonEvent mouseEv) =
    SDL.mouseButtonEventMotion mouseEv == Pressed &&
    isWidgetHovered imgButton (mousePos mouseEv) 
  isWidgetClicked _ _ = False

  updateSelection imgbtn isSelected = do
    writeIORef (ibSelected imgbtn) isSelected

instance WidgetInteraction StaticText where
  updateColor (StaticText _ _ _ colorRef) newColor = do
    writeIORef colorRef newColor 

isPointInRect :: V2 Int -> (Int, Int, Int, Int) -> Bool
isPointInRect (V2 px py) (rx, ry, rw, rh) =
  px >= rx && px <= rx + rw &&
  py >= ry && py <= ry + rh

isMouseOnButton :: V2 Int -> Button -> Bool
isMouseOnButton mpos (Button _ _ (V2 x y) (V2 w h) _ _) = isPointInRect mpos (x, y, w, h)

isButtonClicked :: Button -> SDL.EventPayload -> Bool
isButtonClicked button (MouseButtonEvent mouseEv) =
    SDL.mouseButtonEventMotion mouseEv == Pressed &&
    isMouseOnButton (mousePos mouseEv) button
isButtonClicked _ _ = False

mousePos :: SDL.MouseButtonEventData -> V2 Int
mousePos e =
  let SDL.P (V2 x y) = SDL.mouseButtonEventPos e in
  fromIntegral <$> V2 x y

-- getWidgetID :: Widget -> String 

createImageButton 
  :: String 
  -> SDL.Rectangle CInt
  -> V2 CInt 
  -> V2 CInt 
  -> Int
  -> IO ImageButton
createImageButton bId srcRect pos bsize cardid  = do
  rectRef <- newIORef srcRect 
  cardidRef <- newIORef cardid
  selectedRef <- newIORef False 
  return $ ImageButton bId rectRef pos bsize cardidRef selectedRef 

createTextTexture :: MonadIO m => String -> Color -> Font -> SDL.Renderer -> m Texture
createTextTexture text color font renderer = do
  textSurface <- SDL.Font.solid font color $ pack text
  textTexture <- SDL.createTextureFromSurface renderer textSurface
  SDL.freeSurface textSurface
  return textTexture
  
widgetEvent :: EventSource SDL.EventPayload -> MomentIO (R.Event AppEvent)
widgetEvent sdlSource = do
  e <- fromSDLEvent sdlSource
  return $ buttonClickEvent e

getButtonPosition :: Button -> V2 Int
getButtonPosition (Button _ _ p _ _ _) = p

createButton :: String -> String -> V2 Int -> GColor -> IO Button
createButton text bID pos color = do
  font <- SDL.Font.load "Roboto-Black.ttf" 20
  textSurface <- SDL.Font.solid font fBlack $ pack text
  (V2 tW tH) <- SDL.surfaceDimensions textSurface
  let txt = Ui.Types.Text text font fBlack $ V4 10 10 10 10
  let (V4 w e n s) = _textPadding txt
  let rW = fromIntegral tW + w + e
  let rH = fromIntegral tH + n + s
  colorRef <- newIORef color
  selectRef <- newIORef False
  return $ Button bID (Just txt) pos (V2 rW rH) colorRef selectRef

createNoTextButton :: String -> V2 Int -> V2 Int -> GColor -> IO Button
createNoTextButton bID pos bsize color = do
  colorRef <- newIORef color
  selectRef <- newIORef False
  return $ Button bID Nothing pos bsize colorRef selectRef

sinkBehavior :: (a -> IO ()) -> Behavior a -> MomentIO ()
sinkBehavior updateFunc beh = do
  initialVal <- valueBLater beh
  liftIOLater $ updateFunc initialVal
  e <- changes beh
  reactimate' $ fmap updateFunc <$> e



-- we connect widget to the behavior, when the string changes we update the text
sinkStaticText :: StaticText -> Behavior String -> MomentIO ()
sinkStaticText st = sinkBehavior (updateStaticText st)

-- connecting image button to the behavior
sinkImageButton :: ImageButton -> Behavior Card -> MomentIO ()
sinkImageButton imgb = sinkBehavior (updateImageButtonTile imgb)


getTextBackgroundSize :: Text -> IO (V2 Int)
getTextBackgroundSize text = do
  let (Ui.Types.Text str font _ (V4 w e n s)) = text
  textSurface <- SDL.Font.solid font fBlack $ pack str
  (V2 textW textH) <- SDL.surfaceDimensions textSurface
  let bgWidth = fromIntegral textW + w + e
  let bgHeight = fromIntegral textH + n + s
  return $ V2 bgWidth bgHeight


createStaticText :: String -> String -> V2 Int -> IO StaticText 
createStaticText text stId pos = do
  font <- SDL.Font.load "Roboto-Black.ttf" 20
  let txt = Ui.Types.Text text font fBlack $ V4 10 10 10 10
  textRef <- newIORef txt
  colorRef <- newIORef white

  return $ StaticText stId textRef pos colorRef
  
updateStaticText :: StaticText -> String -> IO ()
updateStaticText staticText newString = do
  oldText <- readIORef (stTextRef staticText)
  writeIORef (stTextRef staticText) $ oldText {_textMsg = newString}

updateImageButtonTile :: ImageButton -> Card -> IO ()
updateImageButtonTile imgb card = do
  writeIORef (ibSrcRect imgb) $ cardToTile card

updateImageButtonCard :: ImageButton -> Int -> IO ()
updateImageButtonCard imgb n = do
  writeIORef (ibCard imgb) n

filterButtons :: [Widget] -> [Button]
filterButtons = concatMap transWidget
  where
    transWidget (WButton b) = [b]
    transWidget _ = []

filterStaticText :: [Widget] -> [StaticText]
filterStaticText = concatMap transWidget
  where
    transWidget (WStaticText st) = [st]
    transWidget _ = []

filterImageButton :: [Widget] -> [ImageButton]
filterImageButton = concatMap transWidget
  where
    transWidget (WImgButton st) = [st]
    transWidget _ = []

{-----------------------------------------------------------------------------
    Events
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

{-
registerButtonClickEvent :: Button -> MomentIO (EventSource AppEvent)
registerButtonClickEvent button = do
  (addHandler, fire) <- newAddHandler
  return 
  -}

hold :: a -> R.Event a -> MomentIO (Behavior a)
hold initial e = accumB initial (const <$> e)

event0 :: Button -> SDLEventSource -> MomentIO (R.Event ())
event0 button sdlSource = do
    clickEvent <- filterE (isButtonClicked button) 
        <$> fromAddHandler (addHandler $ getSDLEvent sdlSource)
    return $ () <$ clickEvent

createSDLEventAddHandler :: IO (EventSource SDL.EventPayload)
createSDLEventAddHandler = newAddHandler

fromSDLEvent :: EventSource SDL.EventPayload -> MomentIO (R.Event SDL.EventPayload)
fromSDLEvent = fromAddHandler . addHandler

sdlEvent :: SDLEventSource -> MomentIO WrappedEvent
sdlEvent = fromAddHandler . addHandler . getSDLEvent

sdlButtonEventToAppEvent :: SDL.EventPayload -> Maybe AppEvent
sdlButtonEventToAppEvent (SDL.MouseButtonEvent e)
  | SDL.mouseButtonEventMotion e == SDL.Pressed =
      Just $ MouseClickEvent (mousePos e)
sdlButtonEventToAppEvent _ = Nothing

buttonClickEvent :: WrappedEvent -> R.Event AppEvent
buttonClickEvent = filterJust . fmap sdlButtonEventToAppEvent


keyEvent :: WrappedEvent -> WrappedEvent
keyEvent = filterE isKey
  where
    isKey e = case e of
      SDL.KeyboardEvent _ -> True
      _                   -> False

keyDownEvent :: WrappedEvent -> R.Event SDL.Keysym
keyDownEvent = filterJust . (isDown <$>) . keyEvent
  where isDown (SDL.KeyboardEvent (KeyboardEventData _ Pressed _ k)) = Just k
        isDown _ = Nothing

-- | Event carrying the key pressed up
keyUpEvent :: WrappedEvent -> R.Event SDL.Keysym
keyUpEvent = filterJust . (isDown <$>) . keyEvent
  where isDown (SDL.KeyboardEvent (KeyboardEventData _ Released _ k)) = Just k
        isDown _ = Nothing

-- | Filter any mouse event (button or move)
mouseEvent :: WrappedEvent -> WrappedEvent
mouseEvent esdl = unionWith f mouseMotion (mouseButtonEvent esdl)
  where
    f _ e2 = e2
    mouseMotion =  filterE isMotion esdl
    isMotion e = case e of
        SDL.MouseMotionEvent MouseMotionEventData {} -> True
        _ -> False

-- | Mouse button event
mouseButtonEvent :: WrappedEvent -> WrappedEvent
mouseButtonEvent = filterE isButton
  where
    isButton e = case e of
        SDL.MouseButtonEvent MouseButtonEventData{} -> True
        _ -> False

-- | Filter an event on a particular key being held down
keyFilter :: SDL.Keycode-> SDL.EventPayload -> Bool
keyFilter k (SDL.KeyboardEvent (KeyboardEventData _ Pressed _ (SDL.Keysym _ k' _ )))
  | k == k'   = True
keyFilter _ _ = False

-- | Filter an event on a particular key being released
keyUpFilter :: SDL.Keycode -> SDL.EventPayload -> Bool
keyUpFilter k (SDL.KeyboardEvent (KeyboardEventData _ Released _ (SDL.Keysym _ k' _ )))
  | k == k'     = True
keyUpFilter _ _ = False

createAppEventSource :: IO (EventSource AppEvent)
createAppEventSource = newAddHandler

isKeyPressEvent :: AppEvent -> Bool
isKeyPressEvent (KeyPressEvent _) = True
isKeyPressEvent _ = False

isMouseClickEvent :: AppEvent -> Bool
isMouseClickEvent (MouseClickEvent _) = True
isMouseClickEvent _ = False

{-isServerMessageEvent :: AppEvent -> Bool
isServerMessageEvent (ServerMessageEvent _) = True
isServerMessageEvent _ = False-}

isButtonClickEvent :: AppEvent -> Bool
isButtonClickEvent (ButtonClickEvent _) = True
isButtonClickEvent _ = False

isButtonEventWithID :: AppEvent -> String -> Bool
isButtonEventWithID (ButtonClickEvent bid) bid' = bid == bid'
isButtonEventWithID _ _ = False

isGameStateEvent :: AppEvent -> Bool
isGameStateEvent (GameStateEvent _) = True
isGameStateEvent _ = False

isToggleCardChoiceEvent :: AppEvent -> Bool
isToggleCardChoiceEvent (ToggleCardChoice _) = True
isToggleCardChoiceEvent _ = False

{-----------------------------------------------------------------------------
    Graphics
------------------------------------------------------------------------------}
intTo8WordColor :: V4 Int -> V4 Word8
intTo8WordColor color = fromIntegral <$> color

toCIntRect :: SDL.Rectangle Int -> SDL.Rectangle CInt
toCIntRect rect =
  let (SDL.Rectangle (SDL.P (V2 x y)) (V2 w h)) = rect in 
  SDL.Rectangle (SDL.P (fromIntegral <$> V2 x y)) (fromIntegral <$> V2 w h)


