{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where



import Control.Monad
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import qualified Data.Vector.Storable as V

import Linear

import qualified SDL

import Graphics.GL

import LearnGL.Window
import LearnGL.Shader
import LearnGL.Texture
import LearnGL.Transform
import LearnGL.GameState
import LearnGL.Camera

import Data.Traversable

import Control.Monad.State.Lazy

import Control.Lens hiding (indices)


thisDir :: FilePath
thisDir = "app/2-Lighting/15-lighting-maps/"

shaderPath :: FilePath
shaderPath = thisDir ++ "shaders/"

textures :: FilePath
textures = "textures/"

wall :: FilePath
wall = textures ++ "wall.jpg"

face :: FilePath
face = textures ++ "awesomeface.png"

vertexPath :: FilePath
vertexPath = shaderPath ++ "vertex.glsl"
fragmentPath :: FilePath
fragmentPath = shaderPath ++ "fragment.glsl"



vertices :: V.Vector GLfloat
vertices = V.fromList
 [  -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,
     0.5, -0.5, -0.5,  0.0,  0.0, -1.0,
     0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
     0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
    -0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
    -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,

    -0.5, -0.5,  0.5,  0.0,  0.0, 1.0,
     0.5, -0.5,  0.5,  0.0,  0.0, 1.0,
     0.5,  0.5,  0.5,  0.0,  0.0, 1.0,
     0.5,  0.5,  0.5,  0.0,  0.0, 1.0,
    -0.5,  0.5,  0.5,  0.0,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0,  0.0, 1.0,

    -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,
    -0.5,  0.5, -0.5, -1.0,  0.0,  0.0,
    -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
    -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
    -0.5, -0.5,  0.5, -1.0,  0.0,  0.0,
    -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,

     0.5,  0.5,  0.5,  1.0,  0.0,  0.0,
     0.5,  0.5, -0.5,  1.0,  0.0,  0.0,
     0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
     0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
     0.5, -0.5,  0.5,  1.0,  0.0,  0.0,
     0.5,  0.5,  0.5,  1.0,  0.0,  0.0,

    -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
     0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
     0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
     0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
    -0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
    -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,

    -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
     0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
     0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
     0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
    -0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
    -0.5,  0.5, -0.5,  0.0,  1.0,  0.0
  ]

indices :: V.Vector GLuint
indices = V.fromList
    [ 0 .. 35
    ]



proxySizeOf :: forall a p. Storable a => p a -> Int
proxySizeOf _ = sizeOf (undefined :: a)

-- floatSize = sizeOf (undefined :: Float)

cube :: IO (GLuint, GLuint)
cube =
  alloca $ \vboPtr -> do
    alloca $ \eboPtr -> do

      glGenBuffers 1 vboPtr
      glGenBuffers 1 eboPtr
      vbo <- peek vboPtr
      ebo <- peek eboPtr

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
      glBindBuffer GL_ARRAY_BUFFER 0
      glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0

      return (vbo, ebo)



makeVao :: GLuint -> GLuint -> GLuint -> IO ()
makeVao vao vbo ebo = do

  glBindVertexArray vao

  glBindBuffer GL_ARRAY_BUFFER vbo
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo

  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
    (fromIntegral (sizeOf (undefined :: Float) * 6))
    nullPtr
  glEnableVertexAttribArray 0

  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE
    (fromIntegral (sizeOf (undefined :: Float) * 6))
    (nullPtr `plusPtr` (sizeOf (undefined ::Float) * 3))
  glEnableVertexAttribArray 1

  -- glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE
  --   (fromIntegral (sizeOf (undefined :: Float) * 9))
  --   (nullPtr `plusPtr` (sizeOf (undefined ::Float) * 7))
  -- glEnableVertexAttribArray 2

  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0



mainState :: StateT GameState IO ()
mainState = do

  window <- liftIO $ openWindow screenWidth screenHeight

  shaderCube <- liftIO $ makeShaderProgram vertexPath fragmentPath
  shaderSource <- liftIO $ makeShaderProgram (shaderPath ++ "light_cube_vs.glsl") (shaderPath ++ "light_cube_fs.glsl")

  viewPos <- liftIO $ glGetUniformLocation shaderCube  =<< newCString "viewPos"

  glActiveTexture GL_TEXTURE0
  _ <- liftIO $ loadTexture wall
  uniTex0 <- liftIO $ glGetUniformLocation shaderCube =<< newCString "texture0"

  glActiveTexture GL_TEXTURE1
  _ <- liftIO $ loadTexture face
  uniTex1 <- liftIO $ glGetUniformLocation shaderCube =<< newCString "texture1"


  lightVao <- liftIO $ alloca $ \vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    peek vaoPtr

  vao <- liftIO $ alloca $ \vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    peek vaoPtr

  (vbo,ebo) <- liftIO cube

  liftIO $ makeVao vao vbo ebo
  liftIO $ makeVao lightVao vbo ebo

  glEnable GL_DEPTH_TEST

  let loop :: StateT GameState IO () = do
          events <- liftIO SDL.pollEvents
          let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

          processCameraEvents

          time <- liftIO SDL.ticks
          lastFrame <- use now

          let timef = (fromIntegral time :: Float) / 1000
              delta = timef - lastFrame
          deltaTime .= delta

          now .= timef
          glClearColor 0.0 0.0 0.0 1.0

          glClear GL_COLOR_BUFFER_BIT
          glClear GL_DEPTH_BUFFER_BIT
          glUseProgram shaderCube

          glUniform1i uniTex0 0
          glUniform1i uniTex1 1

          -- V3 viewx viewy viewz <- use position
          setUniform3f shaderCube "viewPos" =<< use position

          glBindVertexArray vao

          setUniform3f shaderCube "material.ambient" (V3 1 0.5 0.31)
          setUniform3f shaderCube "material.diffuse" (V3 1 0.5 0.31)
          setUniform3f shaderCube "material.specular" (V3 0.5 0.5 0.5)
          setUniformf  shaderCube "material.shininess" 32

          let lightPos = V3 (5 * cos timef) 3 (5 * sin timef)

          setUniform3f shaderCube "light.ambient" (V3 0.2 0.2 0.2)
          setUniform3f shaderCube "light.diffuse" (V3 0.5 0.5 0.5)
          setUniform3f shaderCube "light.specular" (V3 1.0 1.0 1.0)
          setUniform3f  shaderCube "light.position" lightPos


          updateWorld shaderCube


          liftIO $ setMatrix shaderCube "model" (mkTransformation (axisAngle (V3 0 1 1) 0) (V3 0 0 1))

          glDrawElements GL_TRIANGLES  36 GL_UNSIGNED_INT nullPtr

          glUseProgram shaderSource
          glBindVertexArray lightVao

          liftIO $ setMatrix shaderSource "model" (mkTransformation (axisAngle (V3 0 0 0) 0) lightPos)

          updateWorld shaderSource


          glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_INT nullPtr

          liftIO $ SDL.glSwapWindow window

          unless quit loop

  loop
  liftIO $ closeWindow window


main :: IO ()
main = evalStateT mainState defaultState



