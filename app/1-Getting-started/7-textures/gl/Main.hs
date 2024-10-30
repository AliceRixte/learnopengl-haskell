{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import Control.Monad
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import qualified Data.Vector.Storable as V


import qualified SDL
import Graphics.GL

import LearnGL.Window
import LearnGL.Shader
import LearnGL.Texture


thisDir :: FilePath
thisDir = "app/1-Getting-started/7-textures/gl/"

textures :: FilePath
textures = "textures/"

wall :: FilePath
wall = textures ++ "wall.jpg"

face :: FilePath
face = textures ++ "awesomeface.png"

vertexPath :: FilePath
vertexPath = thisDir ++ "shaders/vertex.glsl"
fragmentPath :: FilePath
fragmentPath = thisDir ++ "shaders/fragment.glsl"

vertices :: V.Vector GLfloat
vertices = V.fromList
    [ -0.5, -0.5, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0
    ,  0.5, -0.5, 0.0, 0.0, 1.0, 0.0, 1.0, 0.1, 0.0
    ,  0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 1.0, 0.1, 0.1
    , -0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.1
    ]

indices :: V.Vector GLuint
indices = V.fromList
    [ 0, 1, 2
    , 0, 2, 3
    ]



proxySizeOf :: forall a p. Storable a => p a -> Int
proxySizeOf _ = sizeOf (undefined :: a)

-- floatSize = sizeOf (undefined :: Float)

initBuffers ::  IO (GLuint, GLuint)
initBuffers = do
   alloca $ \vaoPtr -> do
    alloca $ \vboPtr -> do
      alloca $ \eboPtr -> do
        glGenVertexArrays 1 vaoPtr
        glGenBuffers 1 vboPtr
        glGenBuffers 1 eboPtr
        vao <- peek vaoPtr
        vbo <- peek vboPtr
        ebo <- peek eboPtr

        glBindVertexArray vao
        glBindBuffer GL_ARRAY_BUFFER vbo
        V.unsafeWith vertices $ \verticesPtr -> do
          glBufferData GL_ARRAY_BUFFER
                      (fromIntegral (V.length vertices * proxySizeOf vertices))
                      (castPtr verticesPtr)
                      GL_STATIC_DRAW

        glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
        V.unsafeWith indices $ \indicesPtr -> do
          glBufferData GL_ELEMENT_ARRAY_BUFFER
                      (fromIntegral (V.length indices * proxySizeOf indices))
                      (castPtr indicesPtr)
                      GL_STATIC_DRAW

        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
          (fromIntegral (sizeOf (undefined :: Float) * 9))
          nullPtr

        glEnableVertexAttribArray 0


        glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE
          (fromIntegral (sizeOf (undefined :: Float) * 9))
          (nullPtr `plusPtr` (sizeOf (undefined ::Float) * 3))
        glEnableVertexAttribArray 1


        glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE
          (fromIntegral (sizeOf (undefined :: Float) * 9))
          (nullPtr `plusPtr` (sizeOf (undefined ::Float) * 7))
        glEnableVertexAttribArray 2

        glBindBuffer GL_ARRAY_BUFFER 0
        glBindVertexArray 0
        return (vao,vbo)

main :: IO ()
main = do
  window <- openWindow screenWidth screenHeight
  -- glViewport 0 0 (fromIntegral screenWidth) (fromIntegral screenHeight)

  shaderProgram <- makeShaderProgram vertexPath fragmentPath

  ourColorStr <- newCString "ourColor"
  ourColor <- glGetUniformLocation shaderProgram ourColorStr

  glActiveTexture GL_TEXTURE0
  texture0 <- loadTexture wall
  uniTex0 <- glGetUniformLocation shaderProgram =<< newCString "texture0"

  texture1 <- loadTexture wall
  uniTex1 <- glGetUniformLocation shaderProgram =<< newCString "texture1"

  glActiveTexture GL_TEXTURE1
  texture2 <- loadTexture face


  (vao, _) <- initBuffers

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        time <- SDL.ticks

        glClearColor 0.2 0.3 0.3 1.0
        glClear GL_COLOR_BUFFER_BIT
        glUseProgram shaderProgram

        glUniform1i uniTex0 0
        glUniform1i uniTex1 1

        glUniform4f ourColor ((sin (fromIntegral time/ 1000) + 1)/ 2) 0 0 1
        glBindVertexArray vao
        glDrawElements GL_TRIANGLES  6 GL_UNSIGNED_INT nullPtr

        SDL.glSwapWindow window

        unless quit loop

  loop

  closeWindow window

