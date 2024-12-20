module LearnGL.Window
  ( module LearnGL.Window
  ) where

import Control.Monad
import Foreign.C.Types

import SDL (($=))
import qualified SDL
import SDL.Vect
import qualified SDL.Raw as SDLR

core33Config :: SDL.OpenGLConfig
core33Config = SDL.OpenGLConfig
  { SDL.glColorPrecision = V4 8 8 8 0
  , SDL.glDepthPrecision = 24
  , SDL.glStencilPrecision = 8
  , SDL.glMultisampleSamples = 1
  , SDL.glProfile = SDL.Core SDL.Debug 3 3
  }

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

-- | Create a new SDL/OpenGL window
openWindow :: CInt -> CInt -> IO (SDL.Window)
openWindow screenWidth' screenHeight' = do
  _ <- SDLR.glSetAttribute SDLR.SDL_GL_CONTEXT_MAJOR_VERSION 3
  _ <- SDLR.glSetAttribute SDLR.SDL_GL_CONTEXT_MINOR_VERSION 3
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear

  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth' screenHeight',
                         SDL.windowGraphicsContext = SDL.OpenGLContext core33Config}
  SDL.showWindow window
  _ <- SDL.glCreateContext window
  return window

-- | Destroy window and quit SDL
closeWindow :: SDL.Window -> IO ()
closeWindow window = do
  SDL.destroyWindow window
  SDL.quit