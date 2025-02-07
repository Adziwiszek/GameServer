{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Ui.Client (runGraphicsClient) where
import Data.Maybe (listToMaybe)
import Data.IORef
import Control.Concurrent
import Control.Monad.State
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
import Uno.Common.Types
import Uno.Defaults ( defaultCard, defaultSBoard )
import Utils
import Types


chooseCardToShowBehavior :: Behavior Int -> Behavior SBoard ->  Int -> MomentIO (Behavior (Int, Card))
chooseCardToShowBehavior bint bboard  initialIndex = return $
    liftA2 findCardToShow bint bboard  

  where
    findCardToShow :: Int -> SBoard -> (Int, Card)
    findCardToShow offset gs  =
      let index = offset + initialIndex
      in if index < 0 || index >= length (myHand gs)
      then (-1, defaultCard)
      else (index, myHand gs !! index)


-- todo fix card not toggling off after a move

setupReactiveCard
  :: Int
  -> ImageButton
  -> R.Event AppEvent
  -> EventSource AppEvent
  -> Behavior Int
  -> Behavior SBoard
  -> Behavior [Int]
  -> MomentIO ()
setupReactiveCard index imgb appevent eventsource bindex bsboard  bselectedCards= do
  let cardevent = filterE (`isButtonEventWithID` ibID imgb) appevent
      escrollright = filterE (`isButtonEventWithID` "right")  appevent
      escrollleft = filterE (`isButtonEventWithID` "left")  appevent
      etoggledcard = filterE (\case ToggleCardChoice _ -> True; _ -> False) appevent

  -- behavior for changin which card we currently show in this widget  
  cardbehavior <- chooseCardToShowBehavior bindex bsboard index
  sinkImageButton imgb $ fmap snd cardbehavior

  let bcurrnetindex = fmap fst cardbehavior

  reactimate $ fmap toggleShadow $ (,) <$> bcurrnetindex <@> etoggledcard
  reactimate $ fmap fireToggleCardChoice $ bcurrnetindex <@ cardevent
  reactimate $ fmap (scrollShadow 1) $ (,) <$> bcurrnetindex <*> bselectedCards <@ escrollright
  reactimate $ fmap (scrollShadow (-1)) $ (,) <$> bcurrnetindex <*> bselectedCards <@ escrollleft

  where
    fireToggleCardChoice n = do
      when (n >= 0) $ fire eventsource $ ToggleCardChoice n

    scrollShadow delta (currentindex, activecards) = do
      when (delta == 0) $ putStrLn "clicked!!!"
      writeIORef (ibSelected imgb) $ (currentindex + delta) `elem` activecards && (currentindex + delta) >= 0

    toggleShadow (currentIndex, ToggleCardChoice n) = do
      when (currentIndex == n) $ do
        isSelected <- readIORef (ibSelected imgb) 
        putStrLn $ "card " ++ show n ++ " was selected?: " ++ show isSelected
        writeIORef (ibSelected imgb) $ not isSelected
        putStrLn $ "card " ++ show n ++ " is now selected?: " ++ show (not isSelected)
    toggleShadow _ = return ()


setupReactivePlayerInfoBar
  :: Int 
  -> StaticText
  -> EventSource AppEvent
  -> MomentIO ()
setupReactivePlayerInfoBar barIndex playerBar eventsource = do
  appEvent <- fromAddHandler (addHandler eventsource)
  let estartinfo = filterE isInitPlayerBar appEvent 
      egamestateupdate = filterE (\case GameStateEvent _ -> True; _ -> False) appEvent

  -- updating after a move
  bupdate <- hold "no player" $ fmap updatePlayerInfo egamestateupdate
  sinkStaticText playerBar bupdate

  -- start of the game
  binit <- hold "no player" $ fmap getPlayerInfo estartinfo
  sinkStaticText playerBar binit

  where
    getPlayerInfo (InitPlayerBar (_, SPlayer pname _ ncards)) =
      pname ++ ": " ++ show ncards     
    getPlayerInfo _ = undefined

    isInitPlayerBar (InitPlayerBar (id', _)) = barIndex == id'
    isInitPlayerBar  _ = False

    updatePlayerInfo (GameStateEvent gs) = 
      let (SPlayers players) = otherPlayers gs
      in let thisplayer = players !! barIndex
      in splayerName thisplayer ++ ": " ++ show (snumOfCards thisplayer)
    updatePlayerInfo _ = "no player"


runGraphicsClient :: String -> IO ()
runGraphicsClient username = do
  -- Initialize SDL
  initializeAll
  SDL.Font.initialize
  -- Create window and renderer
  window <- createWindow "Uno online!" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  appEventSource <- createAppEventSource
  inchan  :: Chan Message <- newChan 
  outchan :: Chan Message <- newChan
  playerid <- newEmptyMVar
  _ <- forkIO $ startUiClient inchan outchan playerid username

  tileset <- loadTexture renderer "unocards.png"

  let srcRect = cardToTile defaultCard

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


  topCard <- createImageButton "topCard" srcRect (V2 340 200) (V2 100 150) (-1)

  bstartGame <- createButton "start" "startGame" (V2 0 0) white
  bup   <- createButton "<-" "left" (V2 50 450) white
  bdown <- createButton "->" "right" (V2 50 500) white
  bsendmove <- createButton "send" "send" (V2 700 500) white
  name1 <- createStaticText "myname" username (V2 700 420)

  playerBar1 <- createStaticText "player0" "no player" (V2 100 100)
  playerBar2 <- createStaticText "player1" "no player" (V2 100 200)


  let widgets :: [Widget]
      widgets = 
        [WButton bup, WButton bdown, WButton bsendmove,
        WStaticText name1, WImgButton topCard,
        WButton bstartGame
        , WStaticText playerBar1
        , WStaticText playerBar2
        ] ++ map WImgButton handcards



  let networkDescription :: MomentIO ()
      networkDescription = do
        -- Filtering events ===================================================
        appEvent <- fromAddHandler (addHandler appEventSource)
        let eButtonClick = filterE isButtonClickEvent appEvent 
            eup = filterE (`isButtonEventWithID` "right") eButtonClick
            edown = filterE (`isButtonEventWithID` "left") eButtonClick
            estartGame = filterE (`isButtonEventWithID` "startGame") appEvent
            esend = filterE (`isButtonEventWithID` "send") appEvent
            egamestate = filterE isGameStateEvent appEvent
            einitgameinfo = filterE (\case SessionPlayers _ -> True; _ -> False) appEvent

        -- Behaviors ==========================================================
        bselectedCards <- accumB [] $ toggleCardChoice appEvent
        
        cardIndexBehavior <- setupCounter 0 eup edown

        bboard <- hold defaultSBoard $ fmap (\(GameStateEvent gs) -> gs) egamestate
        botherplayers <- hold [] $ fmap (\(SessionPlayers sp) -> sp) einitgameinfo
        btopcard <- hold defaultCard $ fmap topCardFromEvent egamestate 

        -- Making Events and Behaviors actually do stuff ======================
        mapM_ (\(index, imgb) -> setupReactiveCard index imgb appEvent appEventSource cardIndexBehavior bboard bselectedCards)
            $ zip [0..] handcards
        
        setupReactivePlayerInfoBar 0 playerBar1 appEventSource 
        setupReactivePlayerInfoBar 1 playerBar2 appEventSource

        sinkImageButton topCard btopcard

        reactimate $ fmap (`startGame` outchan) estartGame
        -- reactimate $ fmap print (botherplayers <@ esend)
        -- TODO change this to send move to the server
        reactimate $ fmap (\x -> sendGameMove x outchan appEventSource) ((,) <$> bboard <*> bselectedCards <@ esend)


  -- network <- setupNetwork appEventSource 
  network <- compile networkDescription
  actuate network


  eventLoop renderer appEventSource widgets inchan outchan tileset

  where

  startGame :: AppEvent -> Chan Message -> IO ()
  startGame appevent outchan = case appevent of 
    ButtonClickEvent _ -> do
      writeChan outchan $ Message Server (Types.Text ":start ") 0
    _ -> return ()

  topCardFromEvent :: AppEvent -> Card
  topCardFromEvent (GameStateEvent gs) = discardedCard gs
  topCardFromEvent _ = defaultCard

  sendGameMove 
    :: (SBoard, [Int]) 
    -> Chan Message 
    -> EventSource AppEvent 
    -> IO ()
  sendGameMove (gameState, selectedCards) outchan eventsource = do
    let cards = choose selectedCards $ myHand gameState
    putStrLn $ "sending move = " ++ show cards
    -- send players move to the server
    writeChan outchan $ Message Server (GameMove cards) 0
    -- toggle of selected cards
    mapM_ (fire eventsource . ToggleCardChoice) selectedCards
    




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
          fire eventSource $ GameStateEvent gs
          return ()
        Types.StartingGameInfo ginfo -> do
          mapM_ (\(i, p) -> fire eventSource $ InitPlayerBar (i, p)) $ zip [0..] ginfo
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
          renderButtons renderer buttons 
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

