{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Ui.Client (runGraphicsClient) where
import Control.Concurrent
import Control.Monad (when, unless)
import Control.Monad.Fix (fix)
import Control.Exception (bracket, handle, SomeException(..), displayException)
import Foreign.C.Types (CInt)
import SDL
import SDL.Font
import SDL.Image (loadTexture)

import Reactive.Banana as R
import Reactive.Banana.Frameworks 

import Ui.Types
import Ui.Utils
import Ui.Graphics
import Client (startUiClient)
import Types

type PlayerID = MVar Int

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
  zip ([Card (Number i, col) | i <- [0..9]] ++ [Card (Add 2, col), Card (Skip, col), Card (Switch, col)]) $
  zip [colKey, colKey..] [0..]

colorTextureTileMap :: [(Card, (CInt, CInt))]
colorTextureTileMap = 
  let colorMaps = concatMap makeColorMap [Red, Green, Yellow, Blue]
  in colorMaps ++ [(Card (AddColorless (4, Null), Colorless), (0, 4)), (Card (ChangeColor Null, Colorless), (1, 4))]

getTileRect :: (CInt, CInt) -> SDL.Rectangle CInt
getTileRect (x, y) = Rectangle 
          (P (V2 (x * tileWidth) (y * tileHeight)))
          (V2 tileWidth tileHeight)

cardToTile :: Card -> SDL.Rectangle CInt
cardToTile card = case lookup card colorTextureTileMap of
  Just pair -> getTileRect pair
  Nothing -> undefined


--unoCardToTextureTile :: Card -> SDL.Rectangle CInt
--unoCardToTextureTile card = 

runGraphicsClient :: String -> IO ()
runGraphicsClient username = do
  -- Initialize SDL
  initializeAll
  SDL.Font.initialize
  -- Create window and renderer
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  inchan  :: Chan Message <- newChan 
  outchan :: Chan Message <- newChan
  playerid <- newEmptyMVar
  _ <- forkIO $ startUiClient inchan outchan playerid username

  tileset <- loadTexture renderer "unocards.png"

  let c = Card (AddColorless (4, Null), Colorless)
  let srcRect = cardToTile c

  imageDUpa <- createImageButton "imag" srcRect (V2 500 100) (V2 150 200)

  bup   <- createButton "<-" "left" (V2 50 450)
  bdown <- createButton "->" "right" (V2 50 500)
  name1 <- createStaticText "myname" username (V2 400 100)
  st1 <- createStaticText "id1" "0" (V2 200 450)
  st2 <- createStaticText "id1" "1" (V2 300 450)
  st3 <- createStaticText "id1" "2" (V2 400 450)
  st4 <- createStaticText "id1" "3" (V2 500 450)

  let widgets :: [Widget]
      widgets = [WButton bup, WButton bdown, 
        WStaticText st1,WStaticText st2, WStaticText st3,WStaticText st4, WStaticText name1,
        WImgButton imageDUpa]
  appEventSource <- createAppEventSource

  -- sdlEventSource <- SDLEventSource <$> newAddHandler

  let networkDescription :: MomentIO ()
      networkDescription = do
        appEvent <- fromAddHandler (addHandler appEventSource)
        let eButtonClick = filterE isButtonClickEvent appEvent 
        let eup = filterE (`isButtonEventWithID` "right") eButtonClick
        let edown = filterE (`isButtonEventWithID` "left") eButtonClick

        c1 <- setupCounter 0 eup edown
        c2 <- setupCounter 1 eup edown
        c3 <- setupCounter 2 eup edown
        c4 <- setupCounter 3 eup edown

        --reactimate $ fmap showClick eButtonClick
        sinkStaticText st1 (show <$> c1)
        sinkStaticText st2 (show <$> c2)
        sinkStaticText st3 (show <$> c3)
        sinkStaticText st4 (show <$> c4)

  -- network <- setupNetwork appEventSource 
  network <- compile networkDescription
  actuate network


  eventLoop renderer appEventSource widgets inchan outchan tileset


-- Read commands and fire corresponding events 
eventLoop :: 
  Renderer -> 
  EventSource AppEvent -> 
  [Widget] -> 
  Chan Message ->
  Chan Message ->
  Texture ->
  IO ()
eventLoop renderer eventSource widgets inchan outchan textureass = do
    let buttons = filterButtons widgets
    let staticTexts = filterStaticText widgets
    let imgButtons = filterImageButton widgets

    -- todo: in this thread convert server messages to AppEvent
    readerThread <- forkIO $ fix $ \loop -> do
      msg <- readChan inchan
      case content msg of
        Types.Text str -> do
          putStrLn str
        Types.GameState gmst -> return ()
        _ -> return ()
      loop


    let loop = do
          -- Events
          events <- pollEvents
          handleSDLEvent (fire eventSource) buttons events
          let qPressed = any eventIsQPress events

          -- Rendering
          rendererDrawColor renderer $= V4 0 0 255 255
          clear renderer
          renderButtons renderer $ zip buttons [white, white]
          renderStaticTexts renderer staticTexts
          renderImageButtons renderer textureass imgButtons

          present renderer
          unless qPressed loop

    loop


    writeChan outchan $ Message Server (Types.Text "quit ") 0

    killThread readerThread


{-----------------------------------------------------------------------------
    Program logic
------------------------------------------------------------------------------}
handleSDLEvent :: (AppEvent -> IO ()) -> [Button] -> [SDL.Event] -> IO ()
handleSDLEvent fireEvent buttons events = do
  -- Handle key press
  let mouseEvents = collectMousePressedEvents events 
  mapM_ (\e -> executeButtons (mousePos e) buttons) mouseEvents
    
  where
    executeButtons :: V2 Int -> [Button] -> IO ()
    executeButtons p bs = do
      mapM_ (\b -> 
          when 
            (isMouseOnButton p b) 
            (fireEvent $ ButtonClickEvent $ buttonID b)) 
        bs



setupCounter :: Int -> R.Event AppEvent  -> R.Event AppEvent -> MomentIO (Behavior Int)
setupCounter n eup edown =
            accumB n $ unions
            [ (+1) <$ eup
            , subtract 1 <$ edown
            ]

