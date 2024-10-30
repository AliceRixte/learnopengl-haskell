{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified SDL.Internal.Vect as SDLV
-- import SDL.Raw
import Graphics.GL

import LearnGL.Window
import LearnGL.Shader
import LearnGL.Texture
import LearnGL.Transform

import Pandia.Space.Geometry
import Pandia.Space.Geometry.Affine3D

import Data.Traversable

import System.Random.Stateful

import Control.Monad.State.Lazy

import Control.Lens hiding (indices)

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


data GameState = GameState {
    _forwardSpeed :: Float
  , _lateralSpeed :: Float
  , _yaw :: Float
  , _pitch :: Float
  , _fov :: Float
  , _front :: V3 Float
  , _position :: V3 Float
  , _worldUp :: V3 Float
  , _now :: Float
  , _deltaTime :: Float

}

$(makeLenses ''GameState)


defaultState = GameState
  { _forwardSpeed = 0
  , _lateralSpeed = 0
  , _yaw = 0
  , _pitch = 0
  , _fov = pi / 4
  , _front = V3 0 0 (-1)
  , _position = V3 0 0 0
  , _worldUp = V3 0 1 0
  , _now = 0
  , _deltaTime = 0.001
  }

defaultForwardSpeed :: Float
defaultForwardSpeed = 5

defaultLateralSpeed :: Float
defaultLateralSpeed = defaultForwardSpeed

defaultYawSpeed :: Float
defaultYawSpeed = 5

defaultPitchSpeed :: Float
defaultPitchSpeed = defaultYawSpeed

defaultZoomSpeed :: Float
defaultZoomSpeed = 2


updateWorld :: GLuint -> StateT GameState IO ()
updateWorld shaderProgram = do
  fwd <- use forwardSpeed
  lateral <- use lateralSpeed
  frt2 <- use front
  up <- use worldUp
  yw <- use yaw
  ptch <- use pitch
  let frt = normalize (V3 (cos yw * cos ptch) (sin ptch) (sin yw * cos ptch))
  let rightVec = -(cross frt up)
  delta <- use deltaTime
  position += delta * lateral *^ rightVec
  position += delta * fwd *^ frt

  timef <- use now
  pos <- use position


  fov' <- use fov
  liftIO $ setMatrix shaderProgram "view" $ Affine3D $ lookAt pos (pos + frt) up
  liftIO $ setMatrix shaderProgram "projection"  (Affine3D (
    perspective fov' (fromIntegral screenWidth / fromIntegral screenHeight) 0.1 100 ))




-- processDirectionEvents :: (SDL.Scancode -> Bool) -> StateT GameState IO ()
-- processDirectionEvents kbstate= do

--   if kbstate SDL.ScancodeW then
--     forwardSpeed .= defaultForwardSpeed
--   else if kbstate SDL.ScancodeS then
--     forwardSpeed .= -defaultForwardSpeed
--   else
--     forwardSpeed .= 0

--   if kbstate SDL.ScancodeA then
--     lateralSpeed .= defaultLateralSpeed
--   else if kbstate SDL.ScancodeD then
--     lateralSpeed .= -defaultLateralSpeed
--   else
--     lateralSpeed .= 0


processCameraEvents :: StateT GameState IO ()
processCameraEvents = do
  kbstate <- liftIO SDL.getKeyboardState
  modstate <- liftIO SDL.getModState
  if kbstate SDL.ScancodeW then
    forwardSpeed .= defaultForwardSpeed
  else if kbstate SDL.ScancodeS then
    forwardSpeed .= -defaultForwardSpeed
  else
    forwardSpeed .= 0

  if kbstate SDL.ScancodeA then
    lateralSpeed .= defaultLateralSpeed
  else if kbstate SDL.ScancodeD then
    lateralSpeed .= -defaultLateralSpeed
  else
    lateralSpeed .= 0

  delta <- use deltaTime

  if kbstate SDL.ScancodeR then
    fov += defaultZoomSpeed * delta
  else if kbstate SDL.ScancodeF then
    fov -= defaultZoomSpeed * delta
  else
    return ()



mainState :: StateT GameState IO ()
mainState = do

  window <- liftIO $ openWindow screenWidth screenHeight

  shaderProgram <- liftIO $ makeShaderProgram vertexPath fragmentPath

  ourColorStr <- liftIO $ newCString "ourColor"
  ourColor <- glGetUniformLocation shaderProgram ourColorStr

  glActiveTexture GL_TEXTURE0
  _ <- liftIO $ loadTexture wall
  uniTex0 <- liftIO $ glGetUniformLocation shaderProgram =<< newCString "texture0"

  glActiveTexture GL_TEXTURE1
  _ <- liftIO $ loadTexture face
  uniTex1 <- liftIO $ glGetUniformLocation shaderProgram =<< newCString "texture1"

  (vao, _) <- liftIO initBuffers

  glEnable GL_DEPTH_TEST


  let loop :: StateT GameState IO () = do
          events <- liftIO SDL.pollEvents
          let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

          -- kbstate <- liftIO SDL.getKeyboardState
          processCameraEvents
          -- if kbstate SDL.ScancodeLCtrl then
          --   do
          -- if kbstate SDL.ScancodeW then
          --   forwardSpeed .= defaultForwardSpeed
          -- else if kbstate SDL.ScancodeS then
          --   forwardSpeed .= -defaultForwardSpeed
          -- else
          --   forwardSpeed .= 0

          -- if kbstate SDL.ScancodeA then
          --   lateralSpeed .= defaultLateralSpeed
          -- else if kbstate SDL.ScancodeD then
          --   lateralSpeed .= -defaultLateralSpeed
          -- else
          --   lateralSpeed .= 0
          -- else
          --   oldfov <- use fov
          --   let fov' = oldpitch + defaultZoomSpeed * delta * (1 - 2 * fromIntegral mousey / fromIntegral screenHeight)
          --   return ()

          time <- liftIO SDL.ticks
          lastFrame <- use now

          let timef = (fromIntegral time :: Float) / 1000
              delta = timef - lastFrame
          deltaTime .= delta

          SDL.P (V2 mousex mousey) <- SDL.getAbsoluteMouseLocation

          yaw += defaultYawSpeed * delta * (2 * fromIntegral mousex / fromIntegral screenWidth - 1)

          oldpitch <- use pitch
          let ptch = oldpitch + defaultPitchSpeed * delta * (1 - 2 * fromIntegral mousey / fromIntegral screenHeight)
          let newPitch
                | ptch >= pi /2 - 0.01  =  pi/2 - 0.01
                | ptch <= - (pi/2) + 0.01 = -(pi/2) +0.01
                | otherwise             = ptch

          pitch .= newPitch


          now .= timef
          glClearColor 0.2 0.3 0.3 1.0
          glClear GL_COLOR_BUFFER_BIT
          glClear GL_DEPTH_BUFFER_BIT
          glUseProgram shaderProgram


          glUniform1i uniTex0 0
          glUniform1i uniTex1 1

          glUniform4f ourColor ((sin (fromIntegral time/ 3000) + 1)/ 2) 0 0 1

          glBindVertexArray vao


          updateWorld shaderProgram

            -- let gen = random (mkStdGen 2021)
          _ <- for cubePositions (\pos -> do

            liftIO $ setMatrix shaderProgram "model" $ Affine3D $ mkTransformation(axisAngle pos (pi*(fromIntegral time)/2000)) pos

            glDrawElements GL_TRIANGLES  36 GL_UNSIGNED_INT nullPtr
            )
          liftIO $ SDL.glSwapWindow window

          unless quit loop

  loop
  liftIO $ closeWindow window


main :: IO ()
main = evalStateT mainState defaultState



