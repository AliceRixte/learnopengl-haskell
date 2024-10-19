{-# LANGUAGE OverloadedStrings #-}

import SDL
import qualified Graphics.Rendering.OpenGL as GL
import Foreign.C.Types
import Control.Monad (unless)
import Linear (V2(..), V3(..))

-- Initialize SDL and create an OpenGL context
initializeSDL :: IO Window
initializeSDL = do
    -- Initialize all SDL subsystems
    initializeAll


    -- Create an SDL window with OpenGL context
    let windowConfig = defaultWindow
            { windowResizable = True -- Allow window resizing
            , windowGraphicsContext = OpenGLContext SDL.defaultOpenGL } -- Specify OpenGL context
    window <- createWindow "OpenGL with SDL" windowConfig

    -- Create an OpenGL context associated with the window
    _ <- glCreateContext window

    -- Enable VSync (optional)
    -- swapInterval ImmediateUpdates -- Set VSync

    return window

-- Set up OpenGL environment
initializeOpenGL :: IO ()
initializeOpenGL = do
    GL.clearColor $= GL.Color4 0.1 0.2 0.3 1 -- Dark blue background
    GL.viewport $= (GL.Position 0 0, GL.Size 800 600)

-- Render a triangle
renderTriangle :: IO ()
renderTriangle = do
    -- Clear the screen
    GL.clear [GL.ColorBuffer]

    -- Define triangle vertices
    GL.renderPrimitive GL.Triangles $ do
        GL.color $ GL.Color3 1.0 0.0 (0.0 :: GL.GLfloat) -- Red
        GL.vertex $ GL.Vertex3 (-0.5) (-0.5) (0.0 :: GL.GLfloat)
        GL.color $ GL.Color3 0.0 1.0 (0.0 :: GL.GLfloat) -- Green
        GL.vertex $ GL.Vertex3 0.5 (-0.5) (0.0 :: GL.GLfloat)
        GL.color $ GL.Color3 0.0 0.0 (1.0 :: GL.GLfloat) -- Blue
        GL.vertex $ GL.Vertex3 0.0 0.5 (0.0 :: GL.GLfloat)

    -- Flush the OpenGL commands
    GL.flush

-- Main loop
appLoop :: Window -> IO ()
appLoop window = do
    let loop = do
            events <- pollEvents
            let quit = any (== QuitEvent) $ map eventPayload events

            -- Render the triangle
            renderTriangle

            -- Swap buffers to display the result
            glSwapWindow window

            unless quit loop
    loop

-- Main function
main :: IO ()
main = do
    window <- initializeSDL
    initializeOpenGL
    appLoop window
    destroyWindow window
    quit
