--port of https://github.com/bergey/haskell-OpenGL-examples

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

core33Config :: SDL.OpenGLConfig
core33Config = SDL.OpenGLConfig
  { SDL.glColorPrecision = V4 8 8 8 0
  , SDL.glDepthPrecision = 24
  , SDL.glStencilPrecision = 8
  , SDL.glMultisampleSamples = 1
  , SDL.glProfile = SDL.Core SDL.Debug 3 3 
  }

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
          , SDL.windowGraphicsContext = SDL.OpenGLContext core33Config
          , SDL.windowResizable = True
          }
          
  SDL.showWindow window

  _ <- SDL.glCreateContext window

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        GL.clear [GL.ColorBuffer]
        SDL.glSwapWindow window

        unless quit loop

  loop

  SDL.destroyWindow window
  SDL.quit


