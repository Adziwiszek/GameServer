{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Ui.Client (runGraphicsClient) where
import Data.Maybe (listToMaybe)
import Data.IORef
import Control.Concurrent
import Control.Monad.State
import SDL
import SDL.Font
import SDL.Image (loadTexture)

import Reactive.Banana as R
import Reactive.Banana.Frameworks 

import Ui.Types
import Ui.Utils
import Ui.Graphics
import Client (startUiClient)
import Uno.Common.Types
import Uno.Utils
import Uno.Defaults ( defaultCard, defaultSBoard )
import Types
import Utils



dupa :: Behavior Int -> Behavior SBoard -> Int -> MomentIO (Behavior Card)
dupa bint bboard initialIndex = return $
    liftA2 (\offset gs ->
              let index = offset + initialIndex
              in if index < 0
              then defaultCard 
              else maybe defaultCard id (myHand gs !? index)
          ) bint bboard


setupReactiveCard
  :: Int
  -> ImageButton
  -> R.Event AppEvent
  -> Behavior Int
  -> Behavior SBoard
  -> MomentIO ()
setupReactiveCard index imgb appevent bindex bsboard = do
  let cardevent = filterE (`isButtonEventWithID` ibID imgb) appevent
  cardbehavior <- dupa bindex bsboard index
  sinkImageButton imgb cardbehavior
  reactimate $ fmap debugPrintCard (cardbehavior <@ cardevent)



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

  let c = Card (Number 9, Blue)
  let srcRect = cardToTile c

  handcard1 <- createImageButton "handcard1" srcRect (V2 100 420) (V2 100 150) 0
  handcard2 <- createImageButton "handcard2" srcRect (V2 250 420) (V2 100 150) 1
  handcard3 <- createImageButton "handcard3" srcRect (V2 400 420) (V2 100 150) 2
  handcard4 <- createImageButton "handcard4" srcRect (V2 550 420) (V2 100 150) 3

  let handcards = 
        [ handcard1
        , handcard2
        , handcard3
        , handcard4
        ]

  bstartGame <- createButton "start" "startGame" (V2 0 0)

  bup   <- createButton "<-" "left" (V2 50 450)
  bdown <- createButton "->" "right" (V2 50 500)
  name1 <- createStaticText "myname" username (V2 400 100)
  offsettxt <- createStaticText "offset" "0" (V2 300 100)

  let widgets :: [Widget]
      widgets = [WButton bup, WButton bdown, 
        WStaticText name1,
        WButton bstartGame, WStaticText offsettxt] ++ map WImgButton handcards
  appEventSource <- createAppEventSource


  let networkDescription :: MomentIO ()
      networkDescription = do
        appEvent <- fromAddHandler (addHandler appEventSource)
        let eButtonClick = filterE isButtonClickEvent appEvent 
            eup = filterE (`isButtonEventWithID` "right") eButtonClick
            edown = filterE (`isButtonEventWithID` "left") eButtonClick
            estartGame = filterE (`isButtonEventWithID` "startGame") appEvent
            
        
        cardIndexBehavior <- setupCounter 0 eup edown

        bboard <- hold defaultSBoard $ fmap (\(GameStateEvent gs) -> gs) (filterE isGameStateEvent appEvent)

        mapM_ (\(index, imgb) -> setupReactiveCard index imgb appEvent cardIndexBehavior bboard)
            $ zip [0..] handcards

        sinkStaticText offsettxt $ fmap show cardIndexBehavior
        reactimate $ fmap (`startGame` outchan) estartGame

  -- network <- setupNetwork appEventSource 
  network <- compile networkDescription
  actuate network


  eventLoop renderer appEventSource widgets inchan outchan tileset

  where


  startGame :: AppEvent -> Chan Message -> IO ()
  startGame appevent outchan = case appevent of 
    ButtonClickEvent bid -> do
      writeChan outchan $ Message Server (Types.Text ":start ") 0
    _ -> return ()





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
        Types.GameState gs -> do
          putStrLn "received game state"
          print gs
          fire eventSource $ GameStateEvent gs
          return ()
        _ -> return ()
      loop


    let loop = do
          -- Events
          events <- pollEvents
          handleSDLEvent (fire eventSource) widgets events
          let qPressed = any eventIsQPress events

          -- Rendering
          rendererDrawColor renderer $= V4 0 0 255 255
          clear renderer
          renderButtons renderer $ zip buttons [white, white, white]
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
handleSDLEvent :: (AppEvent -> IO ()) -> [Widget] -> [SDL.Event] -> IO ()
handleSDLEvent fireEvent widgets events = do
  let buttons = filterButtons widgets
  let staticTexts = filterStaticText widgets
  let imgButtons = filterImageButton widgets
  -- Handle key press
  let mouseEvents = collectMousePressedEvents events 
  mapM_ (\e -> executeButtons (mousePos e) buttons) mouseEvents
  mapM_ (\e -> executeImgButtons (mousePos e) imgButtons) mouseEvents
    
  where
    executeButtons :: V2 Int -> [Button] -> IO ()
    executeButtons p bs = do
      mapM_ (\b -> 
          when 
            (isMouseOnButton p b) 
            (fireEvent $ ButtonClickEvent $ buttonID b)) 
        bs

    executeImgButtons :: V2 Int -> [ImageButton] -> IO ()
    executeImgButtons p bs = do
      mapM_ (\b -> 
              when 
                (isWidgetHovered b p) 
                (fireEvent $ ButtonClickEvent $ ibID b)
            ) bs


setupCounter :: Int -> R.Event AppEvent  -> R.Event AppEvent -> MomentIO (Behavior Int)
setupCounter n eup edown =
            accumB n $ unions
            [ (+1) <$ eup
            , subtract 1 <$ edown
            ]

