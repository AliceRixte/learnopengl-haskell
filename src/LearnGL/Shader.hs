
module LearnGL.Shader
  ( module LearnGL.Shader
  ) where

import Control.Monad
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc


import GHC.Word

import System.Exit

import Control.Monad.IO.Class

import Linear
import Graphics.GL

setUniformBool :: MonadIO m => GLuint -> String -> Bool -> m ()
setUniformBool shader name True = do
  uni <- liftIO $ glGetUniformLocation shader =<< newCString name
  glUniform1i uni 1
setUniformBool shader name False = do
  uni <- liftIO $ glGetUniformLocation shader =<< newCString name
  glUniform1i uni 0

setUniformi :: (MonadIO m, Integral n) => GLuint -> String -> n -> m ()
setUniformi shader name n = do
  uni <- liftIO $ glGetUniformLocation shader =<< newCString name
  glUniform1i uni (fromIntegral n)

setUniformf :: (MonadIO m) => GLuint -> String -> Float -> m ()
setUniformf shader name f = do
  uni <- liftIO $ glGetUniformLocation shader =<< newCString name
  glUniform1f uni (realToFrac f)

setUniform3f :: (MonadIO m)=> GLuint -> String -> V3 Float -> m ()
setUniform3f shader name (V3 x y z)= do
  uni <- liftIO $ glGetUniformLocation shader =<< newCString name
  glUniform3f uni (realToFrac x) (realToFrac y) (realToFrac z)

setUniform4f :: (MonadIO m) => GLuint -> String -> V4 Float -> m ()
setUniform4f shader name (V4 x y z w)= do
  uni <- liftIO $ glGetUniformLocation shader =<< newCString name
  glUniform4f uni (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w)



compileShader :: GLenum -> FilePath -> IO Word32
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
      putStrLn =<< peekCString infoLogPtr
      when (success <= 0) $ do
        putStrLn "Failed to compile shader "
        exitFailure
  return shader

compileVertexShader :: FilePath -> IO Word32
compileVertexShader = compileShader GL_VERTEX_SHADER

compileFragmentShader :: FilePath -> IO Word32
compileFragmentShader = compileShader GL_FRAGMENT_SHADER

makeShaderProgram :: FilePath -> FilePath -> IO Word32
makeShaderProgram vertexPath fragmentPath = do
  shaderProgram <- glCreateProgram

  vertexShader <- compileVertexShader vertexPath
  fragmentShader <- compileFragmentShader fragmentPath

  glAttachShader shaderProgram vertexShader
  glAttachShader shaderProgram fragmentShader
  glLinkProgram shaderProgram

  alloca $ \successPtr -> do
    glGetProgramiv shaderProgram GL_LINK_STATUS successPtr
    success <- peek successPtr
    when (success <= 0) $ do
      alloca $ \infolog -> do
        glGetShaderInfoLog shaderProgram 512 nullPtr infolog
        putStrLn "ERROR : Shader program linking failed."
        putStrLn =<< peekCString infolog

  return shaderProgram