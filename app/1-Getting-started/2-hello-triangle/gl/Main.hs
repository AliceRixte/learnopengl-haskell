{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import qualified Data.Vector.Storable as V


import qualified SDL
import Graphics.GL

import LearnGL.Window
import LearnGL.Shader



screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

thisDir :: FilePath
thisDir = "app/1-Getting-started/2-hello-triangle/gl/"

vertexPath :: FilePath
vertexPath = thisDir ++ "shaders/vertex.glsl"
fragmentPath :: FilePath
fragmentPath = thisDir ++ "shaders/fragment.glsl"

vertices :: V.Vector GLfloat
vertices = V.fromList [ -0.5, -0.5, 0.0
            ,  0.5, -0.5, 0.0
            ,  0.0,  0.5, 0.0
            ]

proxySizeOf :: forall a p. Storable a => p a -> Int
proxySizeOf _ = sizeOf (undefined :: a)

initBuffers ::  IO (GLuint, GLuint)
initBuffers = do
  alloca $ \vaoPtr -> do
    alloca $ \vboPtr -> do
      glGenVertexArrays 1 vaoPtr
      glGenBuffers 1 vboPtr
      vao <- peek vaoPtr
      vbo <- peek vboPtr

      glBindVertexArray vao
      glBindBuffer GL_ARRAY_BUFFER vbo
      V.unsafeWith vertices $ \verticesPtr -> do
        glBufferData GL_ARRAY_BUFFER
                    (fromIntegral (V.length vertices * proxySizeOf vertices))
                    (castPtr verticesPtr)
                    GL_STATIC_DRAW

        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
          (fromIntegral (sizeOf (undefined :: Float) * 3))
          nullPtr
        glEnableVertexAttribArray 0
        glBindBuffer GL_ARRAY_BUFFER 0
        glBindVertexArray 0
        return (vao,vbo)

main :: IO ()
main = do
  window <- openWindow screenWidth screenHeight
  -- glViewport 0 0 (fromIntegral screenWidth) (fromIntegral screenHeight)

  vertexShader <- compileVertexShader vertexPath
  fragmentShader <- compileFragmentShader fragmentPath

  shaderProgram <- glCreateProgram
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

  (vao, vbo) <- initBuffers

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        glClearColor 0.2 0.3 0.3 1.0
        glClear GL_COLOR_BUFFER_BIT
        glUseProgram shaderProgram
        glBindVertexArray vao
        glDrawArrays GL_TRIANGLES 0 3

        SDL.glSwapWindow window

        unless quit loop

  loop

  closeWindow window

