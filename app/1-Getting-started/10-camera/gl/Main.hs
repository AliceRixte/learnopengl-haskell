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
import Data.Word
import System.Exit

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B

import qualified Data.Vector.Storable as V

import Linear

import qualified SDL
import Graphics.GL

import LearnGL.Window
import LearnGL.Shader
import LearnGL.Texture
import LearnGL.Transform

import Pandia.Space.Geometry
import Pandia.Space.Geometry.Affine3D

import Data.Traversable

import System.Random.Stateful

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

thisDir :: FilePath
thisDir = "app/1-Getting-started/10-camera/gl/"

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
    [ -0.5, -0.5, -0.5, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0
    ,  0.5, -0.5, -0.5, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0
    ,  0.5,  0.5, -0.5, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0
    , -0.5,  0.5, -0.5, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0

    , -0.5, -0.5, 0.5, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0
    ,  0.5, -0.5, 0.5, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0
    ,  0.5,  0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0
    , -0.5,  0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0

    , -0.5, -0.5, -0.5, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0
    ,  0.5, -0.5, -0.5, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0
    , -0.5, -0.5, 0.5, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0
    ,  0.5, -0.5, 0.5, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0

    ,  0.5,  0.5, -0.5, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0
    , -0.5,  0.5, -0.5, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0
    ,  0.5,  0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0
    , -0.5,  0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0

    , -0.5, -0.5, -0.5, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0
    , -0.5, -0.5, 0.5, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0
    , -0.5,  0.5, -0.5, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0
    , -0.5,  0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0

    ,  0.5, -0.5, -0.5, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0
    ,  0.5, -0.5, 0.5, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0
    ,  0.5,  0.5, -0.5, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0
    ,  0.5,  0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0
    ]

indices :: V.Vector GLuint
indices = V.fromList
    [ 0, 1, 2
    , 0, 2, 3
    , 4, 5, 6
    , 4, 6, 7
    , 8, 9, 10
    , 9, 10, 11
    , 12, 13, 14
    , 13, 14, 15
    , 16, 17, 18
    , 17, 18, 19
    , 20, 21, 22
    , 21, 22, 23
    ]


cubePositions :: [V3 GLfloat]
cubePositions =
  [ V3 ( 0.0) ( 0.0) ( 0.1 )
  , V3 ( 2.0) ( 5.0) (-15.0)
  , V3 (-1.5) (-2.2) (-2.5 )
  , V3 (-3.8) (-2.0) (-12.3)
  , V3 ( 2.4) (-0.4) (-3.5 )
  , V3 (-1.7) ( 3.0) (-7.5 )
  , V3 ( 1.3) (-2.0) (-2.5 )
  , V3 ( 1.5) ( 2.0) (-2.5 )
  , V3 ( 1.5) ( 0.2) (-1.5 )
  , V3 (-1.3) ( 1.0) (-1.5 )
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

        -- alloca $ \locationPtr -> do
          -- poke locationPtr (sizeOf (undefined ::Float) * 3)
        glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE
          (fromIntegral (sizeOf (undefined :: Float) * 9))
          (nullPtr `plusPtr` (sizeOf (undefined ::Float) * 3))
        glEnableVertexAttribArray 1

        -- alloca $ \locationPtr -> do
          -- poke locationPtr (sizeOf (undefined ::Float) * 7)
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
  _ <- loadTexture wall
  uniTex0 <- glGetUniformLocation shaderProgram =<< newCString "texture0"

  -- texture1 <- loadTexture wall


  glActiveTexture GL_TEXTURE1
  _ <- loadTexture face
  uniTex1 <- glGetUniformLocation shaderProgram =<< newCString "texture1"

  (vao, _) <- initBuffers


  glEnable GL_DEPTH_TEST

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        time <- SDL.ticks

        glClearColor 0.2 0.3 0.3 1.0
        glClear GL_COLOR_BUFFER_BIT
        glClear GL_DEPTH_BUFFER_BIT
        glUseProgram shaderProgram

        setMatrix shaderProgram "view"  (mx (0) <> mz (-3) <> rz (fromIntegral time / (5000)))
        setMatrix shaderProgram "projection"  (Affine3D (
          perspective 45  (fromIntegral screenWidth / fromIntegral screenHeight) 0.1 100 ))
        glUniform1i uniTex0 0
        glUniform1i uniTex1 1

        glUniform4f ourColor ((sin (fromIntegral time/ 3000) + 1)/ 2) 0 0 1

        glBindVertexArray vao

        -- let gen = random (mkStdGen 2021)
        for cubePositions (\pos -> do
          -- applyAtomicGen
          setMatrix shaderProgram "model" $ Affine3D $ mkTransformation(axisAngle pos (pi*(fromIntegral time)/2000)) pos

          glDrawElements GL_TRIANGLES  36 GL_UNSIGNED_INT nullPtr
          )
        SDL.glSwapWindow window

        unless quit loop

  loop

  closeWindow window

