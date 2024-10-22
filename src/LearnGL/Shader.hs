
module LearnGL.Shader
  ( module LearnGL.Shader
  ) where

import Data.List
import Control.Monad
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import GHC.Word

import System.Exit
import System.IO

import qualified SDL
import Graphics.GL

compileShader :: GLenum -> FilePath -> IO Word32
compileShader shaderType shaderSource = do
  putStrLn "zauhz"
  hFlush stdout
  shader <- glCreateShader shaderType
  blub <- readFile shaderSource
  putStrLn blub
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
      putStrLn =<< peekCString infoLogPtr
      when (success <= 0) $ do
        putStrLn "Failed to compile shader "
        exitFailure
      putStrLn "Compiled shader successfully"
  return shader

compileVertexShader :: FilePath -> IO Word32
compileVertexShader = compileShader GL_VERTEX_SHADER

compileFragmentShader :: FilePath -> IO Word32
compileFragmentShader = compileShader GL_FRAGMENT_SHADER