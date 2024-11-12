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
import LearnGL.Mesh

import Data.Foldable
import Data.Traversable

import Control.Monad.State.Lazy

import Control.Lens hiding (indices)


thisDir :: FilePath
thisDir = "app/3-Model-loading/20-model/"

shaderPath :: FilePath
shaderPath = thisDir ++ "shaders/"

textures :: FilePath
textures = "textures/"

containerDiffuse :: FilePath
containerDiffuse = textures ++ "container2.png"
containerSpecular :: FilePath
containerSpecular = textures ++ "container2_specular.png"


vertexPath :: FilePath
vertexPath = shaderPath ++ "vertex.glsl"
fragmentPath :: FilePath
fragmentPath = shaderPath ++ "fragment.glsl"



cubeVertices :: [Vertex Float]
cubeVertices =
      [
      Vertex (V3 (-0.5) (-0.5) (-0.5)) (V3 ( 0.0) ( 0.0) (-1.0)) (V2 0.0  0.0)
    , Vertex (V3 ( 0.5) (-0.5) (-0.5)) (V3 ( 0.0) ( 0.0) (-1.0)) (V2 1.0  0.0)
    , Vertex (V3 ( 0.5) ( 0.5) (-0.5)) (V3 ( 0.0) ( 0.0) (-1.0)) (V2 1.0  1.0)
    , Vertex (V3 ( 0.5) ( 0.5) (-0.5)) (V3 ( 0.0) ( 0.0) (-1.0)) (V2 1.0  1.0)
    , Vertex (V3 (-0.5) ( 0.5) (-0.5)) (V3 ( 0.0) ( 0.0) (-1.0)) (V2 0.0  1.0)
    , Vertex (V3 (-0.5) (-0.5) (-0.5)) (V3 ( 0.0) ( 0.0) (-1.0)) (V2 0.0  0.0)
    , Vertex (V3 (-0.5) (-0.5) ( 0.5)) (V3 ( 0.0) ( 0.0) ( 1.0)) (V2 0.0  0.0)
    , Vertex (V3 ( 0.5) (-0.5) ( 0.5)) (V3 ( 0.0) ( 0.0) ( 1.0)) (V2 1.0  0.0)
    , Vertex (V3 ( 0.5) ( 0.5) ( 0.5)) (V3 ( 0.0) ( 0.0) ( 1.0)) (V2 1.0  1.0)
    , Vertex (V3 ( 0.5) ( 0.5) ( 0.5)) (V3 ( 0.0) ( 0.0) ( 1.0)) (V2 1.0  1.0)
    , Vertex (V3 (-0.5) ( 0.5) ( 0.5)) (V3 ( 0.0) ( 0.0) ( 1.0)) (V2 0.0  1.0)
    , Vertex (V3 (-0.5) (-0.5) ( 0.5)) (V3 ( 0.0) ( 0.0) ( 1.0)) (V2 0.0  0.0)
    , Vertex (V3 (-0.5) ( 0.5) ( 0.5)) (V3 (-1.0) ( 0.0) ( 0.0)) (V2 1.0  0.0)
    , Vertex (V3 (-0.5) ( 0.5) (-0.5)) (V3 (-1.0) ( 0.0) ( 0.0)) (V2 1.0  1.0)
    , Vertex (V3 (-0.5) (-0.5) (-0.5)) (V3 (-1.0) ( 0.0) ( 0.0)) (V2 0.0  1.0)
    , Vertex (V3 (-0.5) (-0.5) (-0.5)) (V3 (-1.0) ( 0.0) ( 0.0)) (V2 0.0  1.0)
    , Vertex (V3 (-0.5) (-0.5) ( 0.5)) (V3 (-1.0) ( 0.0) ( 0.0)) (V2 0.0  0.0)
    , Vertex (V3 (-0.5) ( 0.5) ( 0.5)) (V3 (-1.0) ( 0.0) ( 0.0)) (V2 1.0  0.0)
    , Vertex (V3 ( 0.5) ( 0.5) ( 0.5)) (V3 ( 1.0) ( 0.0) ( 0.0)) (V2 1.0  0.0)
    , Vertex (V3 ( 0.5) ( 0.5) (-0.5)) (V3 ( 1.0) ( 0.0) ( 0.0)) (V2 1.0  1.0)
    , Vertex (V3 ( 0.5) (-0.5) (-0.5)) (V3 ( 1.0) ( 0.0) ( 0.0)) (V2 0.0  1.0)
    , Vertex (V3 ( 0.5) (-0.5) (-0.5)) (V3 ( 1.0) ( 0.0) ( 0.0)) (V2 0.0  1.0)
    , Vertex (V3 ( 0.5) (-0.5) ( 0.5)) (V3 ( 1.0) ( 0.0) ( 0.0)) (V2 0.0  0.0)
    , Vertex (V3 ( 0.5) ( 0.5) ( 0.5)) (V3 ( 1.0) ( 0.0) ( 0.0)) (V2 1.0  0.0)
    , Vertex (V3 (-0.5) (-0.5) (-0.5)) (V3 ( 0.0) (-1.0) ( 0.0)) (V2 0.0  1.0)
    , Vertex (V3 ( 0.5) (-0.5) (-0.5)) (V3 ( 0.0) (-1.0) ( 0.0)) (V2 1.0  1.0)
    , Vertex (V3 ( 0.5) (-0.5) ( 0.5)) (V3 ( 0.0) (-1.0) ( 0.0)) (V2 1.0  0.0)
    , Vertex (V3 ( 0.5) (-0.5) ( 0.5)) (V3 ( 0.0) (-1.0) ( 0.0)) (V2 1.0  0.0)
    , Vertex (V3 (-0.5) (-0.5) ( 0.5)) (V3 ( 0.0) (-1.0) ( 0.0)) (V2 0.0  0.0)
    , Vertex (V3 (-0.5) (-0.5) (-0.5)) (V3 ( 0.0) (-1.0) ( 0.0)) (V2 0.0  1.0)
    , Vertex (V3 (-0.5) ( 0.5) (-0.5)) (V3 ( 0.0) ( 1.0) ( 0.0)) (V2 0.0  1.0)
    , Vertex (V3 ( 0.5) ( 0.5) (-0.5)) (V3 ( 0.0) ( 1.0) ( 0.0)) (V2 1.0  1.0)
    , Vertex (V3 ( 0.5) ( 0.5) ( 0.5)) (V3 ( 0.0) ( 1.0) ( 0.0)) (V2 1.0  0.0)
    , Vertex (V3 ( 0.5) ( 0.5) ( 0.5)) (V3 ( 0.0) ( 1.0) ( 0.0)) (V2 1.0  0.0)
    , Vertex (V3 (-0.5) ( 0.5) ( 0.5)) (V3 ( 0.0) ( 1.0) ( 0.0)) (V2 0.0  0.0)
    , Vertex (V3 (-0.5) ( 0.5) (-0.5)) (V3 ( 0.0) ( 1.0) ( 0.0)) (V2 0.0  1.0)
    ]

cubeIndices :: V.Vector GLuint
cubeIndices = V.fromList
    [ 0 .. 35
    ]

cubeMesh :: Mesh
cubeMesh = Mesh
  { meshVertices = cubeVertices
  , meshIndices = [0 .. 35]
  , meshTextures = []
  }

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

lightPositions :: [V3 GLfloat]
lightPositions = [
        V3   0.7   0.2     2.0
      , V3   2.3  (-3.3) (-4.0 )
      , V3 (-4.0)  2.0   (-12.0)
      , V3   0.0   0.0   (-3.0 )
  ]


-- floatSize = sizeOf (undefined :: Float)

cube :: IO (GLuint, GLuint, GLuint)
cube = do
  MeshGL vao vbo ebo <- genMeshGL
  bindMesh cubeMesh (MeshGL vao vbo ebo)
  return (vao,vbo, ebo)



makeVao :: GLuint -> GLuint -> GLuint -> IO ()
makeVao vao vbo ebo = do

  glBindVertexArray vao

  glBindBuffer GL_ARRAY_BUFFER vbo
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo

  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
    (fromIntegral (sizeOf (undefined :: Float) * 8))
    nullPtr
  glEnableVertexAttribArray 0

  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE
    (fromIntegral (sizeOf (undefined :: Float) * 8))
    (nullPtr `plusPtr` (sizeOf (undefined ::Float) * 3))
  glEnableVertexAttribArray 1

  glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE
    (fromIntegral (sizeOf (undefined :: Float) * 8))
    (nullPtr `plusPtr` (sizeOf (undefined ::Float) * 6))
  glEnableVertexAttribArray 2

  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0

scaleMat :: Float -> M44 Float
scaleMat s = V4
  (V4 s 0 0 0)
  (V4 0 s 0 0)
  (V4 0 0 s 0)
  (V4 0 0 0 1)


mainState :: StateT GameState IO ()
mainState = do

  window <- liftIO $ openWindow screenWidth screenHeight

  shaderCube <- liftIO $ makeShaderProgram vertexPath fragmentPath
  shaderSource <- liftIO $ makeShaderProgram (shaderPath ++ "light_cube_vs.glsl") (shaderPath ++ "light_cube_fs.glsl")

  viewPos <- liftIO $ glGetUniformLocation shaderCube  =<< newCString "viewPos"

  glActiveTexture GL_TEXTURE0
  diffuseMap <- liftIO $ loadTexture containerDiffuse
  specularMap <- liftIO $ loadTexture containerSpecular

  lightVao <- liftIO $ alloca $ \vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    peek vaoPtr

  -- vao <- liftIO $ alloca $ \vaoPtr -> do
  --   glGenVertexArrays 1 vaoPtr
  --   peek vaoPtr

  MeshGL vao vbo ebo <- liftIO genMeshGL
  liftIO $ bindMesh cubeMesh (MeshGL vao vbo ebo)


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

          setUniformi shaderCube "material.diffuse" 0
          setUniformi shaderCube "material.specular" 1

          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D diffuseMap

          glActiveTexture GL_TEXTURE1
          glBindTexture GL_TEXTURE_2D specularMap

          setUniform3f shaderCube "viewPos" =<< use position

          glBindVertexArray vao

          setUniform3f shaderCube "material.ambient" (V3 1 0.5 0.31)
          setUniform3f shaderCube "material.diffuse" (V3 1 0.5 0.31)
          setUniform3f shaderCube "material.specular" (V3 0.5 0.5 0.5)
          setUniformf  shaderCube "material.shininess" 32

          let lightPos = V3 (5 * cos timef) (0) (5 * sin timef)


          _ <- forAccumM 0 lightPositions (\(i :: Int) pos -> do
              setUniform3f shaderCube
                ("pointLights[" ++ show i ++ "].ambient") (V3 0.2 0.2 0.2)
              setUniform3f shaderCube
                ("pointLights[" ++ show i ++ "].diffuse") (V3 0.7 0.7 0.7)
              setUniform3f shaderCube
                ("pointLights[" ++ show i ++ "].specular") (V3 1.0 1.0 1.0)
              setUniform3f shaderCube
                ("pointLights[" ++ show i ++ "].position") pos
              setUniformf  shaderCube
                ("pointLights[" ++ show i ++ "].constant") 1
              setUniformf  shaderCube
                ("pointLights[" ++ show i ++ "].linear") 0.14
              setUniformf  shaderCube
                ("pointLights[" ++ show i ++ "].quadratic") 0.07
              return(i+1, ())
            )


          setUniform3f shaderCube "spotLight.ambient" (V3 0.2 0.2 0.2)
          setUniform3f shaderCube "spotLight.diffuse" (V3 0.7 0.7 0.7)
          setUniform3f shaderCube "spotLight.specular" (V3 1.0 1.0 1.0)
          setUniform3f shaderCube "spotLight.position" =<< use position
          setUniform3f shaderCube "spotLight.direction" =<< use front
          setUniformf  shaderCube "spotLight.constant" 1
          setUniformf  shaderCube "spotLight.linear" 0
          setUniformf  shaderCube "spotLight.quadratic" 0
          setUniformf  shaderCube "spotLight.cutOff" (cos (pi/16))
          setUniformf  shaderCube "spotLight.outerCutOff" (cos (pi/14))

          setUniform3f shaderCube "dirLight.ambient" (V3 0.1 0.05 0.05)
          setUniform3f shaderCube "dirLight.diffuse" (V3 0.2 0.05 0.05)
          setUniform3f shaderCube "dirLight.specular" (V3 0.5 0.2 0.2)
          setUniform3f shaderCube "dirLight.direction" (V3 0 (-1) 0)

          updateWorld shaderCube

          for_ cubePositions (\pos -> do
            liftIO $ setMatrix shaderCube "model" $
                mkTransformation(axisAngle pos (pi* fromIntegral time/2000)) pos

            glDrawElements GL_TRIANGLES  36 GL_UNSIGNED_INT nullPtr
            )


          glUseProgram shaderSource
          glBindVertexArray lightVao

          updateWorld shaderSource

          for_ lightPositions (\pos -> do
            liftIO $ setMatrix shaderSource "model" (
                mkTransformation(axisAngle pos 0) pos !*! scaleMat 0.1)

            glDrawElements GL_TRIANGLES  36 GL_UNSIGNED_INT nullPtr
            )

          -- liftIO $ setMatrix shaderSource "model" (mkTransformation (axisAngle (V3 0 0 0) 0) lightPos)




          -- glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_INT nullPtr

          liftIO $ SDL.glSwapWindow window

          unless quit loop

  loop
  liftIO $ closeWindow window


main :: IO ()
main = evalStateT mainState defaultState



