{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Foreign.C.Types
import SDL.Vect

import SDL (($=))
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL



screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear

  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "Learn OpenGL"
      SDL.defaultWindow {
            SDL.windowInitialSize = V2 screenWidth screenHeight
          , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
          , SDL.windowResizable = True
          }

  SDL.showWindow window

  _ <- SDL.glCreateContext window

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        GL.clearColor $= GL.Color4 0.2 0.3 0.3 1.0
        GL.clear [GL.ColorBuffer]
        SDL.glSwapWindow window

        unless quit loop

  loop

  SDL.destroyWindow window
  SDL.quit


