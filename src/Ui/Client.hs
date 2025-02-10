{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Ui.Client (runGraphicsClient) where
import Data.IORef
import Control.Concurrent
import Control.Monad.State
import Control.Concurrent.STM
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
import Uno.Utils
import Utils
import Types


changeButtonColor :: Button -> GColor -> IO ()
changeButtonColor btn col = do
    let colorRef = buttonColor btn
    writeIORef colorRef col


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
setupReactiveCard index imgb appevent eventsource bindex bsboard bselectedCards= do
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
      writeIORef (ibSelected imgb) $ (currentindex + delta) `elem` activecards && (currentindex + delta) >= 0

    toggleShadow (currentIndex, ToggleCardChoice n) = do
      when (currentIndex == n) $ do
        isSelected <- readIORef (ibSelected imgb) 
        writeIORef (ibSelected imgb) $ not isSelected
    toggleShadow _ = return ()


setupReactivePlayerInfoBar
  :: Int 
  -> StaticText
  -> EventSource AppEvent
  -> Behavior SBoard
  -> MomentIO ()
setupReactivePlayerInfoBar barIndex playerBar eventsource bsboard = do
  appEvent <- fromAddHandler (addHandler eventsource)
  let estartinfo = filterE isInitPlayerBar appEvent 
      egamestateupdate = filterE (\case GameStateEvent _ -> True; _ -> False) appEvent


  -- updating after a move
  bupdate <- hold "no player" $ fmap updatePlayerInfo egamestateupdate
  sinkStaticText playerBar bupdate

  -- start of the game
  binit <- hold "no player" $ fmap getPlayerInfo estartinfo
  sinkStaticText playerBar binit

  sinkBehavior (updateBGColor playerBar) bsboard

  where
    getPlayerInfo (InitPlayerBar (_, SPlayer pname _ ncards)) =
      pname ++ ": " ++ show ncards     
    getPlayerInfo _ = undefined

    isInitPlayerBar (InitPlayerBar (id', _)) = barIndex == id'
    isInitPlayerBar  _ = False

    updatePlayerInfo (GameStateEvent gs) = 
      let (SPlayers players) = otherPlayers gs
      in if barIndex >= length players
        then "no player"
        else 
          let thisplayer = players !! barIndex
          in splayerName thisplayer ++ ": " ++ show (snumOfCards thisplayer)
    updatePlayerInfo _ = "no player"

    updateBGColor st sboard = do
      let colorRef = stBgColor st
      let (cid, _) = currentPlayerInfo sboard
      writeIORef colorRef $ 
        if cid == barIndex 
          then white
          else grey
        
      

setupColorChoice
  :: Button
  -> GColor
  -> EventSource AppEvent
  -> MomentIO ()
setupColorChoice btn color eventsource = do
  appEvent <- fromAddHandler (addHandler eventsource)
  let ebuttonclicked = filterE (`isButtonEventWithID` showColor color) appEvent 
      echangedcolor = filterE (\case ChangeColorEvent _ -> True; _ -> False) appEvent

  reactimate $ fmap fireChooseColor ebuttonclicked
  reactimate $ fmap changeShadow echangedcolor 
  
  where
    changeShadow (ChangeColorEvent newcol) = do
      updateSelection btn $ color == newcol
    changeShadow _ = return ()
    
    fireChooseColor _ = do
      fire eventsource $ ChangeColorEvent color


runGraphicsClient :: String -> IO ()
runGraphicsClient username = do
  -- Initialize SDL
  initializeAll
  SDL.Font.initialize
  -- Create window and renderer
  window <- createWindow "Uno online!" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  appEventSource <- createAppEventSource
  inchan  <- atomically newTChan 
  outchan <- atomically newTChan
  playerid <- newEmptyMVar
  _ <- forkIO $ startUiClient inchan outchan playerid username

  tileset <- loadTexture renderer "unocards.png"

  let srcRect = cardToTile defaultCard

  handcard1 <- createImageButton "handcard1" srcRect (V2 80 420) (V2 100 150) 0
  handcard2 <- createImageButton "handcard2" srcRect (V2 230 420) (V2 100 150) 1
  handcard3 <- createImageButton "handcard3" srcRect (V2 380 420) (V2 100 150) 2
  handcard4 <- createImageButton "handcard4" srcRect (V2 530 420) (V2 100 150) 3

  let handcards = 
        [ handcard1
        , handcard2
        , handcard3
        , handcard4
        ]


  topCard <- createImageButton "topCard" srcRect (V2 340 200) (V2 100 150) (-1)

  bstartGame <- createButton "start" "startGame" (V2 0 0) white
  bup   <- createButton "<-" "left" (V2 20 450) white
  bdown <- createButton "->" "right" (V2 20 500) white

  txtcurrentColor <- createStaticText "current color" "currentColor" (V2 100 4) 
  btncurrentColor <- createNoTextButton "currentColorBtn" (V2 250 0) (V2 50 50) black

  txtcurrentPlayer <- createStaticText "current player: " "currentPlayer" (V2 350 4)

  name1     <- createStaticText username "myname"  (V2 650 360)
  bsendmove <- createButton "Send move" "send" (V2 650 420) white
  bdrawCard <- createButton "Draw card(s)" "draw" (V2 650 480) white
  bendturn  <- createButton "End turn" "endturn" (V2 650 540) white

  playerBar1 <- createStaticText "no player" "player0"  (V2 100 90)
  playerBar2 <- createStaticText "no player" "player1"  (V2 100 170)
  playerBar3 <- createStaticText "no player" "player2"  (V2 100 250)
  playerBar4 <- createStaticText "no player" "player3"  (V2 100 330)

  let colorChoicePositions = [V2 630 180, V2 700 180, V2 630 250, V2 700 250]
      colorsToChoose = [red, green, blue, yellow]
      colorAndPosition = zip colorsToChoose colorChoicePositions
      colorIds = map showColor colorsToChoose

  colorChoiceButtons :: [Button] <- forM (zip colorIds colorAndPosition) $
    \(bid, (color, pos)) -> createNoTextButton bid pos (V2 50 50) color

  let widgets :: [Widget]
      widgets = 
        [WButton bup, WButton bdown, WButton bsendmove,
        WStaticText name1, WImgButton topCard,
        WButton bstartGame
        , WStaticText playerBar1
        , WStaticText playerBar2
        , WStaticText playerBar3
        , WStaticText playerBar4
        , WButton bdrawCard
        , WButton bendturn
        , WStaticText txtcurrentColor
        , WButton btncurrentColor 
        , WStaticText txtcurrentPlayer
        ] ++ map WImgButton handcards
        ++ map WButton colorChoiceButtons



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
            edrawcard = filterE (`isButtonEventWithID` "draw") appEvent
            eendturn = filterE (`isButtonEventWithID` "endturn") appEvent

        -- Behaviors ==========================================================
        bselectedCards <- accumB [] $ toggleCardChoice appEvent

        bcurrentPlayerName <- hold "_" $ 
            (\(GameStateEvent gs) -> "current player: " ++ snd (currentPlayerInfo gs))
            <$> egamestate
        sinkStaticText txtcurrentPlayer bcurrentPlayerName
        {-
         -  TODO
         -  change current color button based on this event
         -
         - -}
         
          
        bcurrentColor <- hold black $ (\(GameStateEvent st) -> getCurrentColor st) <$> egamestate
        sinkBehavior (changeButtonColor btncurrentColor) bcurrentColor
        
        cardIndexBehavior <- setupCounter 0 eup edown

        bboard <- hold defaultSBoard $ fmap (\(GameStateEvent gs) -> gs) egamestate
        botherplayers <- hold [] $ fmap (\(SessionPlayers sp) -> sp) einitgameinfo
        btopcard <- hold defaultCard $ fmap topCardFromEvent egamestate 

        -- Making Events and Behaviors actually do stuff ======================
        mapM_ (\(index, imgb) -> setupReactiveCard index imgb appEvent appEventSource cardIndexBehavior bboard bselectedCards)
            $ zip [0..] handcards
        
        mapM_ (\(index, pbar) ->
            setupReactivePlayerInfoBar index pbar appEventSource bboard)
            $ zip [0..] [playerBar1, playerBar2, playerBar3, playerBar4]

        mapM_ (\(btn, color) -> setupColorChoice btn color appEventSource) $ zip colorChoiceButtons colorsToChoose

        sinkImageButton topCard btopcard

        reactimate $ fmap (`startGame` outchan) estartGame
        reactimate $ fmap (\x -> sendDrawCard x outchan appEventSource) $ bselectedCards <@ edrawcard
        reactimate $ fmap (\x -> sendGameMove x outchan appEventSource) $ (,) <$> bboard <*> bselectedCards <@ esend
        reactimate $ fmap (\x -> sendEndTurn x outchan) eendturn



  -- network <- setupNetwork appEventSource 
  network <- compile networkDescription
  actuate network


  eventLoop renderer appEventSource widgets inchan outchan tileset

  where

  startGame :: AppEvent -> TChan Message -> IO ()
  startGame appevent outchan = case appevent of 
    ButtonClickEvent _ -> do
      atomically $ writeTChan outchan $ Message Server (Types.Text ":start ") 0
    _ -> return ()

  topCardFromEvent :: AppEvent -> Card
  topCardFromEvent (GameStateEvent gs) = discardedCard gs
  topCardFromEvent _ = defaultCard

  sendGameMove 
    :: (SBoard, [Int]) 
    -> TChan Message 
    -> EventSource AppEvent 
    -> IO ()
  sendGameMove (gameState, selectedCards) outchan eventsource = do
    putStrLn $ "card ids = " ++ show selectedCards
    putStrLn $ "my hand = " ++ show (myHand gameState)
    let cards = choose selectedCards $ myHand gameState
    putStrLn $ "sending move = " ++ show cards
    -- send players move to the server
    atomically $ writeTChan outchan $ Message Server (GameMove cards) 0
    -- toggle of selected cards
    mapM_ (fire eventsource . ToggleCardChoice) selectedCards
    
  sendDrawCard selectedCards outchan eventsource = do
    putStrLn "drawing card!"
    atomically $ writeTChan outchan 
      $ Message Server (GameMove [Card (SelfDraw, Null)]) 0
    mapM_ (fire eventsource . ToggleCardChoice) selectedCards

  sendEndTurn _ outchan = do
    atomically $ writeTChan outchan 
      $ Message Server (GameMove [Card (EndTurn, Null)]) 0


-- Read commands and fire corresponding events 
eventLoop :: 
  Renderer -> 
  EventSource AppEvent -> 
  [Widget] -> 
  TChan Message ->
  TChan Message ->
  Texture ->
  IO ()
eventLoop renderer eventSource widgets inchan outchan textureass = do
    let buttons = filterButtons widgets
    let staticTexts = filterStaticText widgets
    let imgButtons = filterImageButton widgets

    -- select starting color
    fire eventSource $ ChangeColorEvent red

    -- todo: in this thread convert server messages to AppEvent
    readerThread <- forkIO $ fix $ \loop -> do
      msg <- atomically $ readTChan inchan
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
          rendererDrawColor renderer $= V4 61 107 87 255
          clear renderer
          renderButtons renderer buttons 
          renderStaticTexts renderer staticTexts
          renderImageButtons renderer textureass imgButtons

          present renderer
          unless qPressed loop

    loop


    atomically $ writeTChan outchan $ Message Server (Types.Text "quit ") 0

    mapM_ Ui.Utils.destroyTexture buttons
    mapM_ Ui.Utils.destroyTexture staticTexts
    mapM_ Ui.Utils.destroyTexture imgButtons
    SDL.destroyTexture textureass
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

