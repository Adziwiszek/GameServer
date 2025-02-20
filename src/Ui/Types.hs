module Ui.Types (module Ui.Types) where

import Reactive.Banana as R
import Reactive.Banana.Frameworks
import Foreign.C.Types (CInt)
import Data.Word
import Data.IORef
import SDL
import SDL.Font

import Types
import Uno.Common.Types

{- Events ===================================================================-}
type EventSource a = (AddHandler a, a -> IO ())
type WrappedEvent = R.Event SDL.EventPayload
type TickEvent = R.Event Word32
newtype SDLEventSource = SDLEventSource
  { getSDLEvent  :: EventSource SDL.EventPayload
  }

data AppEvent
  = KeyPressEvent SDL.Keycode
  | ButtonClickEvent String
  | MouseClickEvent (V2 Int)
  | ServerMessage Message
  | GameStateEvent SBoard
  | ToggleCardChoice Int
  | SessionPlayers [SPlayer]
  | InitPlayerBar (Int, SPlayer)
  | ChangeColorEvent GColor

{- Graphics =================================================================-}
type Screen = SDL.Surface
newtype Graphic = Graphic { paintGraphic :: Screen -> IO () }

type GColor = V4 Int

data Text = Text 
  { _textMsg     :: String
  , _textFont    :: Font
  , _textColor   :: Color 
  , _textPadding :: V4 Int
  }

data Button = Button
  { buttonID   :: String
  , buttonText :: Maybe Text
  , buttonPos  :: V2 Int
  , buttonSize :: V2 Int
  , buttonColor :: IORef GColor
  , buttonSelected :: IORef Bool
  , buttonTextTexture :: IORef (Maybe SDL.Texture)
  }
  
data ImageButton = ImageButton
  { ibID   :: String
  , ibSrcRect :: IORef (SDL.Rectangle CInt)
  , ibPos  :: V2 CInt
  , ibSize :: V2 CInt
  , ibCard :: IORef Int
  , ibSelected :: IORef Bool
  }

data StaticText = StaticText
  { stID      :: String
  , stTextRef :: IORef Text 
  , stBackgroundRect :: IORef (SDL.Rectangle Int)
  , stTexture :: IORef (Maybe SDL.Texture)
  , stPos     :: V2 Int
  , stBgColor :: IORef GColor
  }

data Widget
  = WButton Button
  | WStaticText StaticText
  | WImgButton ImageButton

