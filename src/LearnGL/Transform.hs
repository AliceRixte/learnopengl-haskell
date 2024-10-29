module LearnGL.Transform
  ( module LearnGL.Transform
  ) where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import qualified Data.Vector.Storable as V
import qualified Data.Foldable          as F

import Pandia.Space.Geometry.Affine3D

import Graphics.GL

import Linear

withPtr :: (Storable a, Floating a, Foldable f, Foldable g)
  => f (g a)
  -> (Ptr a -> IO ())
  -> IO ()
withPtr
  = V.unsafeWith
  . V.fromList
  . concatMap F.toList

setMatrix :: GLuint -> String -> Affine3D Float -> IO ()
setMatrix id' name (Affine3D val) = do
  withCString name $ \cstr -> do
    location <- glGetUniformLocation id' cstr
    withPtr val (glUniformMatrix4fv location 1 GL_TRUE)