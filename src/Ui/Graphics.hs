module Ui.Graphics (module Ui.Graphics) where 

import Control.Monad (when)
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
renderButton renderer (Button _ (Text msg font fcolor padding) (V2 x y) (V2 rW rH) color) = do
  --SDL.rendererDrawColor renderer $= intTo8WordColor color
  --let rect = SDL.Rectangle (SDL.P )  
  fillRoundRectangle renderer (V2 (fromIntegral x) (fromIntegral y)) (V2 (fromIntegral $ x + rW) (fromIntegral $ y + rH)) 8 $ intTo8WordColor color
  --SDL.fillRect renderer (Just $ toCIntRect rect)
  let (V4 w _ n _) = padding
  textSurface <- SDL.Font.solid font fcolor $ pack msg
  textTexture <- SDL.createTextureFromSurface renderer textSurface
  SDL.freeSurface textSurface
  renderTexture renderer textTexture $ V2 (x + w) (y + n)
  SDL.destroyTexture textTexture

renderTexture :: SDL.Renderer -> SDL.Texture -> V2 Int -> IO ()
renderTexture renderer texture p = do
  textureInfo <- SDL.queryTexture texture
  let tW = SDL.textureWidth textureInfo
      tH = SDL.textureHeight textureInfo
  let destRect = SDL.Rectangle (SDL.P p) (V2 (fromIntegral tW) (fromIntegral tH))
  SDL.copy renderer texture Nothing (Just $ toCIntRect destRect)

renderStaticText :: SDL.Renderer -> StaticText -> IO ()
renderStaticText renderer st = do
  SDL.rendererDrawColor renderer $= stBgColor st
  text <- readIORef (stTextRef st)
  let (Text msg font color padding) = text
  let (V2 x y) = stPos st
      (V4 w _ n _) = padding

  (V2 bgW bgH) <- getTextBackgroundSize text

  let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 bgW bgH) 
  SDL.fillRect renderer (Just $ toCIntRect rect)

  textSurface <- SDL.Font.solid font color $ pack msg
  textTexture <- SDL.createTextureFromSurface renderer textSurface
  SDL.freeSurface textSurface
  renderTexture renderer textTexture $ V2 (x + w) (y + n)
  SDL.destroyTexture textTexture

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



