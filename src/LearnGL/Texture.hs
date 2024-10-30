module LearnGL.Texture
  (module LearnGL.Texture)
  where


import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word
import           Data.List              (isSuffixOf)

import qualified Data.Vector.Storable as V

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B

import           Codec.Picture
import qualified Codec.Picture.Types    as CPTI
import           Codec.Picture.Extra    (flipVertically)
import           Codec.Picture.Types

import System.Exit

import Graphics.GL

toImage :: String
  -> ByteString
  -> (Ptr Word8 -> GLint -> GLint -> IO ())
  -> IO ()
toImage name bytes go =
  case decodeImage bytes of
    Right (ImageYCbCr8 i) ->
      V.unsafeWith (imageData (convertImage i :: CPTI.Image PixelRGB8)) $ \ptr ->
        go ptr (fromIntegral (imageWidth i)) (fromIntegral (imageHeight i))
    Right (ImageRGBA8 i) ->
      V.unsafeWith (imageData (flipVertically (convertImage i :: CPTI.Image PixelRGBA8))) $ \ptr ->
        go ptr (fromIntegral (imageWidth i)) (fromIntegral (imageHeight i))
    _  -> do
      putStrLn name
      exitFailure


loadTexture :: String -> IO Word32
loadTexture name  = do
  alloca $ \texturePtr -> do
    glGenTextures 1 texturePtr
    texture <- peek texturePtr

    glBindTexture GL_TEXTURE_2D texture
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    bytes <- B.readFile name
    let typ | ".png" `isSuffixOf` name = GL_RGBA | otherwise = GL_RGB
    toImage name bytes $ \ptr w h -> do
      glTexImage2D GL_TEXTURE_2D 0
        (fromIntegral typ) w h
          0 typ GL_UNSIGNED_BYTE
            (castPtr ptr)
      glGenerateMipmap GL_TEXTURE_2D
    return texture