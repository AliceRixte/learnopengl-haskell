--port of https://github.com/bergey/haskell-OpenGL-examples

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import Foreign.Storable (sizeOf)
import Foreign.Ptr (nullPtr)
import Foreign.C.Types

import qualified SDL
import Graphics.Rendering.OpenGL

import Window


-- | This file trie to mirror as closely as possible the following code https://learnopengl.com/code_viewer_gh.php?code=src/1.getting_started/2.1.hello_triangle/hello_triangle.cpp

-- See other files in the hellotriangle directory that makes better use of the
-- functionnalities offered by Haskell's OpenGL library.

vertexShaderSource :: BS.ByteString
vertexShaderSource = BS.intercalate "\n"
  [ "#version 330 core\n"
  , "layout (location = 0) in vec3 aPos;\n"
  , "void main()\n"
  , "{\n"
  , "   gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n"
  , "}\0"
  ]

fragmentShaderSource :: BS.ByteString
fragmentShaderSource = BS.intercalate "\n"
  [ "#version 330 core\n"
  , "out vec4 FragColor;\n"
  , "void main()\n"
  , "{\n"
  , "   FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n"
  , "}\n\0"
  ]

vertices :: V.Vector GLfloat
vertices = V.fromList [ -0.5, -0.5, 0.0
            ,  0.5, -0.5, 0.0
            ,  0.0,  0.5, 0.0
            ]

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800,600)

main :: IO ()
main = do
  window <- openWindow screenWidth screenHeight
  viewport $= (Position 0 0, Size (fromIntegral screenWidth) (fromIntegral screenHeight))

  -------------------------- Compile and link shaders --------------------------

  -- vertexShader = glCreateShader(GL_VERTEX_SHADER);
  vertexShader <- createShader VertexShader
  -- glShaderSource(vertexShader, 1, &vertexShaderSource, NULL);
  shaderSourceBS vertexShader $= vertexShaderSource
  -- glCompileShader(vertexShader);
  compileShader vertexShader
  -- glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);
  vertexShaderOK <- get $ compileStatus vertexShader
  unless vertexShaderOK $ do
    -- glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);
      infoLog <- get (shaderInfoLog vertexShader)
      fail ("ERROR::SHADER::VERTEX::COMPILATION_FAILED\n" ++ infoLog)

  -- Compile frament shader

  fragmentShader <- createShader FragmentShader
  shaderSourceBS fragmentShader $= fragmentShaderSource
  compileShader fragmentShader
  fragmentShaderOK <- get $ compileStatus fragmentShader
  unless fragmentShaderOK $ do
      infoLog <- get (shaderInfoLog vertexShader)
      fail ("ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n" ++ infoLog)

  -- Program out of those shaders

  program <- createProgram
  attachShader program vertexShader
  attachShader program fragmentShader
  -- attribLocation program "coord2d" $= AttribLocation 0
  linkProgram program
  linkOK <- get $ linkStatus program
  validateProgram program
  status <- get $ validateStatus program
  unless (linkOK && status) $ do
      infoLog <- get $ programInfoLog program
      fail ("ERROR::SHADER::PROGRAM::LINKING_FAILED\n" ++ infoLog)

  -------------------------------- VAO and VBO ---------------------------------


  vao <- genObjectName
  -- glGenVertexArrays(1, &VAO);
  vbo <- genObjectName
  -- glGenBuffers(1, &VBO);
  bindVertexArrayObject $= Just vao
  -- glBindVertexArray(VAO);
  bindBuffer ArrayBuffer $= Just vbo
  -- glBindBuffer(GL_ARRAY_BUFFER, VBO);


  V.unsafeWith vertices $ \ptr -> do
        let size = fromIntegral (V.length vertices * sizeOf (undefined :: GLfloat))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  -- glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  vertexAttribPointer (AttribLocation 0) $=
        (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  -- glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), (void*)0);

  vertexAttribArray (AttribLocation 0) $= Enabled
  -- glEnableVertexAttribArray(0);

  bindBuffer ArrayBuffer $= Nothing
  -- glBindBuffer(GL_ARRAY_BUFFER, 0);
  bindVertexArrayObject $= Nothing
  -- glBindVertexArray(0);

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        clearColor $= Color4 0.2 0.3 0.3 1
        -- glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
        clear [ColorBuffer]
        -- glClear(GL_COLOR_BUFFER_BIT);



        currentProgram $= Just program
        -- glUseProgram(shaderProgram);

        bindVertexArrayObject $= Just vao
        -- glBindVertexArray(VAO);

        drawArrays Triangles 0 3
        -- glDrawArrays(GL_TRIANGLES, 0, 3);

        SDL.glSwapWindow window

        unless quit loop

  loop
  closeWindow window



