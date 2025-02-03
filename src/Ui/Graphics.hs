module Ui.Graphics (module Ui.Graphics) where 

import Data.IORef
import Data.Text (pack)
import Foreign.C.Types (CInt)
import SDL
import SDL.Font
-- import qualified SDL.Primitive

import Ui.Types
import Ui.Utils

-- Rendering ==================================================================
renderButtons :: Renderer -> [(Button, V4 Int)] -> IO ()
renderButtons renderer buttons = do
  mapM_ helpRender buttons

  where
    helpRender (b, col) = do
      SDL.rendererDrawColor renderer $= intTo8WordColor col
      renderButton renderer b

-- Render a single button
renderButton :: Renderer -> Button -> IO ()
renderButton renderer (Button _ (Text msg font color padding) (V2 x y) (V2 rW rH)) = do
  let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 rW rH) 
  SDL.fillRect renderer (Just $ toCIntRect rect)
  let (V4 w _ n _) = padding
  textSurface <- SDL.Font.solid font color $ pack msg
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
  let destRect = Just $ Rectangle
          (P (ibPos imBut))
          (ibSize imBut)
  copy renderer texture (Just srcRect) destRect

-- Utils =====================================================================

centerTextInRect :: SDL.Rectangle CInt -> (CInt, CInt) -> SDL.Rectangle CInt
centerTextInRect (SDL.Rectangle (SDL.P (V2 rX rY)) (V2 rW rH)) (textW, textH) =
  let x = rX + (rW - textW) `div` 2
      y = rY + (rH - textH) `div` 2
  in SDL.Rectangle (SDL.P (V2 x y)) (V2 (textW `div` 2) (textH `div` 2))



