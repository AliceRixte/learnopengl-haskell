module Window
  ( module Window
  ) where

import Control.Monad
import Foreign.C.Types

import SDL (($=))
import qualified SDL
import SDL.Vect

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)


-- core33Config :: SDL.OpenGLConfig
-- core33Config = SDL.OpenGLConfig
--   { SDL.glColorPrecision = V4 8 8 8 0
--   , SDL.glDepthPrecision = 24
--   , SDL.glStencilPrecision = 8
--   , SDL.glMultisampleSamples = 1
--   , SDL.glProfile = SDL.Core SDL.Debug 3 3
--   }

-- | Create a new SDL/OpenGL window
openWindow :: IO (SDL.Window)
openWindow = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear

  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight,
                         SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL}
  SDL.showWindow window
  _ <- SDL.glCreateContext window
  return window

-- | Destroy window and quit SDL
closeWindow :: SDL.Window -> IO ()
closeWindow window = do
  SDL.destroyWindow window
  SDL.quit