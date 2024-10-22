{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List
import Control.Monad
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Exit

import qualified SDL
import Graphics.GL

import LearnGL.Window



screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)


compileShader :: GLenum -> FilePath -> IO ()
compileShader shaderType shaderSource = do
  shader <- glCreateShader shaderType
  shaderSourceC <- newCString =<< readFile shaderSource
  alloca $ \ shaderSourcePtr -> do
    shaderSourcePtr `poke` shaderSourceC
    glShaderSource shader 1 shaderSourcePtr nullPtr
    glCompileShader shader
  alloca $ \successPtr ->
    alloca $ \infoLogPtr -> do
      glGetShaderiv shader GL_COMPILE_STATUS successPtr
      success <- peek successPtr
      glGetShaderInfoLog shader 512 nullPtr infoLogPtr
      mapM_ print =<< lines <$> peekCString infoLogPtr
      when (success <= 0) $ do
        putStrLn "Failed to compile shader "
        exitFailure
      putStrLn "Compiled shader successfully"

compileVertexShader :: FilePath -> IO ()
compileVertexShader = compileShader GL_VERTEX_SHADER

compileFragmentShader :: FilePath -> IO ()
compileFragmentShader = compileShader GL_FRAGMENT_SHADER

main :: IO ()
main = do
  window <- openWindow screenWidth screenHeight
  glViewport 0 0 (fromIntegral screenWidth) (fromIntegral screenHeight)

  vertexShader <- compileVertexShader "vertex.glsl"
  fragmentShader <- compileFragmentShader "fragment.glsl"

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        glClearColor 0.2 0.3 0.3 1.0
        glClear GL_COLOR_BUFFER_BIT
        SDL.glSwapWindow window

        unless quit loop

  loop

  closeWindow window

