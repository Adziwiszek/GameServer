module Ui.Graphics (module Ui.Graphics) where 

import Control.Monad (when)
import Control.Exception (bracket)
import Data.IORef
import Data.Text (pack)
import Foreign.C.Types (CInt)
import SDL
import SDL.Primitive 
import SDL.Font
-- import qualified SDL.Primitive

import Ui.Types
import Ui.Utils

-- Rendering ==================================================================
renderButtons :: Renderer -> [Button] -> IO ()
renderButtons renderer buttons = do
  mapM_ (renderButton renderer) buttons

-- Render a single button
renderButton :: Renderer -> Button -> IO ()
renderButton renderer btn = do
  let text = buttonText btn
      (V2 x y) = buttonPos btn
      (V2 rW rH) = buttonSize btn
      colorRef = buttonColor btn
      selectedRef = buttonSelected btn

  isSelected <- readIORef selectedRef
  when isSelected drawShadow

  bgcolor <- readIORef colorRef
  fillRoundRectangle renderer 
      (fromIntegral <$> V2  x  y)
      (fromIntegral <$> V2  (x + rW) (y + rH)) 
      8 
      $ intTo8WordColor bgcolor

  case text of
    Just (Text msg font fcolor padding) -> do
      let (V4 w _ n _) = padding
      cachedTexture <- readIORef $ buttonTextTexture btn
      texture <- case cachedTexture of
        Just t -> return t
        Nothing -> do
          surface <- SDL.Font.solid font fcolor $ pack msg
          tex <- SDL.createTextureFromSurface renderer surface
          SDL.freeSurface surface
          writeIORef (buttonTextTexture btn) (Just tex)
          return tex
      renderTexture renderer texture $ V2 (x + w) (y + n)
    Nothing -> return ()
  
  where
    drawShadow :: IO ()
    drawShadow = do 
      let shadowThickness = 10
          (V2 x y) = buttonPos btn
          (V2 w h) = buttonSize btn
          topleft = fromIntegral <$> V2 (x - shadowThickness) (y - shadowThickness)
          bottomright = fromIntegral <$> V2 (x + w + shadowThickness) (y + h + shadowThickness)

      fillRoundRectangle renderer topleft bottomright 20 $ intTo8WordColor black

renderTexture :: SDL.Renderer -> SDL.Texture -> V2 Int -> IO ()
renderTexture renderer texture p = do
  textureInfo <- SDL.queryTexture texture
  let tW = SDL.textureWidth textureInfo
      tH = SDL.textureHeight textureInfo
  let destRect = SDL.Rectangle (SDL.P p) $ fromIntegral <$> V2 tW  tH
  SDL.copy renderer texture Nothing (Just $ toCIntRect destRect)

renderStaticText :: SDL.Renderer -> StaticText -> IO ()
renderStaticText renderer st = do
  bgcolor <- readIORef $ stBgColor st
  SDL.rendererDrawColor renderer $= intTo8WordColor bgcolor
  text <- readIORef (stTextRef st)
  let (Text msg font color padding) = text
      (V2 x y) = stPos st
      (V4 w _ n _) = padding

  rect <- readIORef (stBackgroundRect st)
  SDL.fillRect renderer (Just $ toCIntRect rect)

  cachedTexture <- readIORef $ stTexture st
  texture <- case cachedTexture of
    Just t -> return t
    Nothing -> do
      textSurface <- SDL.Font.solid font color $ pack msg
      textTexture <- SDL.createTextureFromSurface renderer textSurface
      SDL.freeSurface textSurface
      writeIORef (stTexture st) $ Just textTexture
      return textTexture
  renderTexture renderer texture $ V2 (x + w) (y + n)

renderStaticTexts :: SDL.Renderer -> [StaticText] -> IO ()
renderStaticTexts renderer = mapM_ (renderStaticText renderer)

renderImageButtons :: SDL.Renderer -> SDL.Texture -> [ImageButton] -> IO ()
renderImageButtons renderer texture = mapM_ (renderImageButton renderer texture)

renderImageButton :: SDL.Renderer -> SDL.Texture -> ImageButton -> IO ()
renderImageButton renderer texture imBut = do
  srcRect <- readIORef $ ibSrcRect imBut
  selected <- readIORef $ ibSelected imBut
  let destRect = Just $ Rectangle
          (P (ibPos imBut))
          (ibSize imBut)

  when selected drawShadow

  copy renderer texture (Just srcRect) destRect

  where
    drawShadow = do 
      let shadowThickness = 10
          (V2 x y) = ibPos imBut
          (V2 w h) = ibSize imBut
          topleft = V2 (x - shadowThickness) (y - shadowThickness)
          bottomright = V2 (x + w + shadowThickness) (y + h + shadowThickness)

      fillRoundRectangle renderer topleft bottomright 20 $ intTo8WordColor black
      


-- Utils =====================================================================

centerTextInRect :: SDL.Rectangle CInt -> (CInt, CInt) -> SDL.Rectangle CInt
centerTextInRect (SDL.Rectangle (SDL.P (V2 rX rY)) (V2 rW rH)) (textW, textH) =
  let x = rX + (rW - textW) `div` 2
      y = rY + (rH - textH) `div` 2
  in SDL.Rectangle (SDL.P (V2 x y)) (V2 (textW `div` 2) (textH `div` 2))



