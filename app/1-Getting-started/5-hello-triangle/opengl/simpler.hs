--port of https://github.com/bergey/haskell-OpenGL-examples

{-# LANGUAGE CPP #-}
module Main(main) where

import Control.Monad
import System.Exit
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import           System.IO

import SDL (($=))
import qualified SDL
import Graphics.Rendering.OpenGL

import LearnGL.Window

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

vertices :: V.Vector Float
vertices = V.fromList [ -0.5,  0.5, 0.0
                      ,  0.5, -0.5, 0.0
                      ,  0.0,  0.5, 0.0
                      ]

main :: IO ()
main = do

  window <- openWindow screenWidth screenHeight
  (prog, attrib) <- compileShaders
  descr <- initResources
  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        draw (prog, attrib) descr
        SDL.glSwapWindow window

        unless quit loop

  loop

  closeWindow window

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initResources :: IO Descriptor
initResources = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vertices = [
        Vertex2 (-0.90) (-0.90),  -- Triangle 1
        Vertex2   0.85  (-0.90),
        Vertex2 (-0.90)   0.85 ,
        Vertex2   0.90  (-0.85),  -- Triangle 2
        Vertex2   0.90    0.90 ,
        Vertex2 (-0.85)   0.90 ] :: [Vertex2 GLfloat]
      numVertices = length vertices

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  (program,vPosition) <- compileShaders
  currentProgram $= Just program

  let firstIndex = 0
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
  vertexAttribArray vPosition $= Enabled

  return $ Descriptor triangles firstIndex (fromIntegral numVertices)

compileShaders :: IO (Program, AttribLocation)
compileShaders = do
    -- Compile vertex shader
    vs <- createShader VertexShader
    shaderSourceBS vs $= vsSource
    compileShader vs
    vsOK <- get $ compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        exitFailure

    -- Compile frament shader
    fs <- createShader FragmentShader
    shaderSourceBS fs $= fsSource
    compileShader fs
    fsOK <- get $ compileStatus fs
    unless fsOK $ do
        hPutStrLn stderr "Error in fragment shader\n"
        exitFailure

    -- Program out of those shaders
    program <- createProgram
    attachShader program vs
    attachShader program fs
    attribLocation program "coord2d" $= AttribLocation 0
    linkProgram program
    linkOK <- get $ linkStatus program
    validateProgram program
    status <- get $ validateStatus program
    unless (linkOK && status) $ do
        hPutStrLn stderr "linkProgram error"
        plog <- get $ programInfoLog program
        putStrLn plog
        exitFailure
    currentProgram $= Just program

    return (program, AttribLocation 0)

draw :: (Program, AttribLocation) -> Descriptor -> IO ()
draw (program, attrib) (Descriptor triangles firstIndex numVertices) = do
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer]
    viewport $= (Position 0 0, Size (fromIntegral screenWidth) (fromIntegral screenHeight))

    currentProgram $= Just program
    bindVertexArrayObject $= Just triangles
    drawArrays Triangles firstIndex numVertices
    -- let attrib = AttribLocation 0
    -- vertexAttribArray attrib $= Enabled
    -- V.unsafeWith vertices $ \ptr ->
    --     vertexAttribPointer attrib $=
    --       (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
    -- drawArrays Triangles 0 3 -- 3 is the number of vertices
    -- vertexAttribArray attrib $= Disabled

vsSource, fsSource :: BS.ByteString

vsSource = BS.intercalate "\n"
           [
            "#version 330 core",
            "layout (location = 0) in vec3 aPos;"
           , "void main(){"
           , "  gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);"
           , "}"
           ]

fsSource = BS.intercalate "\n"
           [
            "#version 330 core"
           , "out vec4 FragColor;"
           , "void main(){"
           , "FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);"
           , "}"
           ]



