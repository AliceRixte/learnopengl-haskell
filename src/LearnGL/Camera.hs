module LearnGL.Camera
  ( module LearnGL.Camera
  ) where

-- import Foreign.C.String
-- import Foreign.Ptr
-- import Foreign.Marshal.Alloc
-- import Foreign.Storable

-- import qualified Data.Vector.Storable as V
-- import qualified Data.Foldable          as F

-- import Pandia.Space.Geometry.Affine3D

-- import Graphics.GL

-- import Linear

-- data Camera = Camera
--   { position
--   , front
--   , up
--   , right
--   , worldUp :: V3 Float
--   }

-- defaultCamera :: Camera
-- defaultCamera = let
--     position = V3 0 0 0
--     front     = V3 0 0 (-1)
--     right     = normalize (cross front worldUp)
--     up        = normalize (cross up front)
--     worldUp   = V3 0 0 (-1)
--   in
--   Camera
--   { position  = V3 0 0 0
--   , front     = V3 0 0 (-1)
--   , right     = normalize (cross front worldUp)
--   , up        = normalize (cross up front)
--   , worldUp   = V3 0 0 (-1)
--   }