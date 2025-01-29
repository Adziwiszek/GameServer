module Ui.Types (module Ui.Types) where

import Reactive.Banana as R
import Reactive.Banana.Frameworks
import Data.Word
import Data.IORef
import SDL
import SDL.Font

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
  deriving (Show)

{- Graphics =================================================================-}
type Screen = SDL.Surface
newtype Graphic = Graphic { paintGraphic :: Screen -> IO () }

data Text = Text 
  { _textMsg     :: String
  , _textFont    :: Font
  , _textColor   :: Color 
  , _textPadding :: V4 Int
  }

data Button = Button
  { buttonID   :: String
  , buttonText :: Text
  , buttonPos  :: V2 Int
  , buttonSize :: V2 Int
  }

data StaticText = StaticText
  { stID      :: String
  , stTextRef :: IORef Text 
  , stPos     :: V2 Int
  , stSize    :: V2 Int
  , stBgColor :: Color
  }

data Widget
  = WButton Button
  | WStaticText StaticText

