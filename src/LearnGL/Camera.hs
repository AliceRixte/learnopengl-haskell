module LearnGL.Camera
  ( module LearnGL.Camera
  ) where

import LearnGL.GameState
import LearnGL.Window
import LearnGL.Transform

import Control.Monad.State.Lazy
import Control.Lens
import Linear
import qualified SDL

import Graphics.GL


processCameraEvents :: StateT GameState IO ()
processCameraEvents = do
  -- Moving
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
  oldfov <- use fov
  -- Zooming
  let fov'
        | kbstate SDL.ScancodeR = oldfov - defaultZoomSpeed * delta
        | kbstate SDL.ScancodeF = oldfov + defaultZoomSpeed * delta
        | otherwise = oldfov

  let newfov
        | fov' < 1 = 1
        | fov' > 200 = 200
        | otherwise = fov'

  fov .= newfov

  -- Mouse movement

  SDL.P (V2 mousex mousey) <- SDL.getAbsoluteMouseLocation
  yaw .= pi  * (1 - 2 * fromIntegral mousex / fromIntegral screenWidth)

  oldpitch <- use pitch
  let ptch = oldpitch + defaultPitchSpeed * (1 - 2 * fromIntegral mousey / fromIntegral screenHeight)
  let newPitch
        | ptch >= pi /2 - 0.01  =  pi/2 - 0.01
        | ptch <= - (pi/2) + 0.01 = -(pi/2) +0.01
        | otherwise             = ptch

  pitch .= pi / 2 * (2 * fromIntegral mousey / fromIntegral screenHeight - 1)



updateWorld :: GLuint -> StateT GameState IO ()
updateWorld shaderProgram = do
  fwd <- use forwardSpeed
  lateral <- use lateralSpeed
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
  liftIO $ setMatrix shaderProgram "view" $ lookAt pos (pos + frt) up
  liftIO $ setMatrix shaderProgram "projection"  (
    perspective fov' (fromIntegral screenWidth / fromIntegral screenHeight) 0.1 100 )