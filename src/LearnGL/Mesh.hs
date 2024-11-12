{-# LANGUAGE ScopedTypeVariables #-}

module LearnGL.Mesh where

import Foreign

import Linear
import qualified Data.Vector.Storable as V

import Graphics.GL


data Vertex s = Vertex
  { vertexPosition :: V3 s
  , vertexNormal :: V3 s
  , vertexTexture :: V2 s
  }

data Texture = Texture
  { textureId :: Int
  , texturType :: String
  }

data Mesh = Mesh
  { meshVertices :: [Vertex GLfloat]
  , meshIndices :: [Int]
  , meshTextures :: [Texture]
  }

data MeshGL = MeshGL
  { meshVao :: Word32
  , meshVbo :: GLuint
  , meshEbo :: GLuint
  }

genMeshGL :: IO MeshGL
genMeshGL =
  alloca $ \vaoPtr ->
    alloca $ \vboPtr ->
     alloca $ \eboPtr -> do
      glGenVertexArrays 1 vaoPtr
      vao <- peek vaoPtr
      glGenBuffers 1 vboPtr
      vbo <- peek vboPtr
      glGenBuffers 1 eboPtr
      ebo <- peek eboPtr
      return (MeshGL vao vbo ebo)

vertexToFloats :: Real s => Vertex s -> [GLfloat]
vertexToFloats (Vertex (V3 px py pz) (V3 nx ny nz) (V2 tx ty)) =
  fmap realToFrac [px, py, pz, nx, ny, nz, tx, ty]

verticesToFloats :: Real s => [Vertex s] -> [GLfloat]
verticesToFloats =
  foldMap vertexToFloats

proxySizeOf :: forall a p. Storable a => p a -> Int
proxySizeOf _ = sizeOf (undefined :: a)

bindMesh:: Mesh -> MeshGL -> IO ()
bindMesh (Mesh vs is _) (MeshGL vao vbo ebo) = do

  glBindVertexArray vao

  -- glBindBuffer GL_ARRAY_BUFFER vbo
  -- glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo

  -- glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
  --   (fromIntegral (sizeOf (undefined :: Float) * 8))
  --   nullPtr
  -- glEnableVertexAttribArray 0

  -- glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE
  --   (fromIntegral (sizeOf (undefined :: Float) * 8))
  --   (nullPtr `plusPtr` (sizeOf (undefined ::Float) * 3))
  -- glEnableVertexAttribArray 1

  -- glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE
  --   (fromIntegral (sizeOf (undefined :: Float) * 8))
  --   (nullPtr `plusPtr` (sizeOf (undefined ::Float) * 6))
  -- glEnableVertexAttribArray 2

  -- glBindBuffer GL_ARRAY_BUFFER 0
  -- glBindVertexArray 0


  glBindBuffer GL_ARRAY_BUFFER vbo
  let vsFloats = V.fromList $ verticesToFloats vs
  V.unsafeWith vsFloats $ \verticesPtr -> do
    glBufferData GL_ARRAY_BUFFER
                (fromIntegral (V.length vsFloats * proxySizeOf vsFloats))
                (castPtr verticesPtr)
                GL_STATIC_DRAW

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  let cubeIndices = V.fromList (fmap fromIntegral is :: [GLuint])
  V.unsafeWith cubeIndices $ \indicesPtr -> do
    glBufferData GL_ELEMENT_ARRAY_BUFFER
                (fromIntegral (V.length cubeIndices * proxySizeOf cubeIndices))
                (castPtr indicesPtr)
                GL_STATIC_DRAW


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
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
  glBindVertexArray 0

