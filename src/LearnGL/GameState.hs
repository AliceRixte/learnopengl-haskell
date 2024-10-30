{-# LANGUAGE TemplateHaskell #-}

module LearnGL.GameState
  ( module LearnGL.GameState
  ) where

import Linear
import Control.Lens

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