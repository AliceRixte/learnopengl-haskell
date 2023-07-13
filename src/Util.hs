module Util (core33Config) where

import qualified SDL
import SDL.Vect

core33Config :: SDL.OpenGLConfig
core33Config = SDL.OpenGLConfig
  { SDL.glColorPrecision = V4 8 8 8 0
  , SDL.glDepthPrecision = 24
  , SDL.glStencilPrecision = 8
  , SDL.glMultisampleSamples = 1
  , SDL.glProfile = SDL.Core SDL.Debug 3 3 
  }