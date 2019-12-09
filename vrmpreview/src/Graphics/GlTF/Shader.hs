{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module
    Graphics.GlTF.Shader
where

import RIO hiding (first, second)
import RIO.Vector ((!?))
import qualified RIO.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified RIO.Text as T
import qualified RIO.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified RIO.Vector as V
import qualified RIO.Vector.Unsafe as V
import qualified RIO.Vector.Partial as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable.Mutable as SMV
import Control.Monad.Trans.Maybe
import Graphics.Holz.System hiding (registerTextures, Texture)
import Graphics.GL
import Linear hiding (trace)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as J
import qualified Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (with)
import Foreign.C.String
import Foreign.Storable
import Foreign.Storable.Generic
import qualified Codec.Picture as Jp
import Text.RawString.QQ (r)
import Data.GlTF

import Graphics.GlTF.Type

makeVRMShader :: MonadIO m => m MRShader
makeVRMShader = liftIO $
  do
    vertexShader <- glCreateShader GL_VERTEX_SHADER
    fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
    compileShader vertexShaderSource vertexShader
    compileShader fragmentShaderSource fragmentShader

    shaderProg <- glCreateProgram
    glAttachShader shaderProg vertexShader
    glAttachShader shaderProg fragmentShader

    withCString "in_Position" $ glBindAttribLocation shaderProg 0
    withCString "in_UV" $ glBindAttribLocation shaderProg 1
    withCString "in_Normal" $ glBindAttribLocation shaderProg 2

    glLinkProgram shaderProg
    glUseProgram shaderProg

    linked <- overPtr (glGetProgramiv shaderProg GL_LINK_STATUS)
    when (linked == GL_FALSE) $ do
        maxLength <- overPtr (glGetProgramiv shaderProg GL_INFO_LOG_LENGTH)
        allocaArray (fromIntegral maxLength) $ \ptr -> do
            glGetProgramInfoLog shaderProg maxLength nullPtr ptr
            BS.packCString ptr >>= BS.putStr

    locationModel <- getUniform shaderProg "model"
    locationProjection <- getUniform shaderProg "projection"
    locationSight <- getUniform shaderProg "sight"
    locationBaseColorFactor <- getUniform shaderProg "baseColorFactor"
    locationPlaneDir <- getUniform shaderProg "planeDir"
    locationLamFactor <- getUniform shaderProg "lamFactor"
    locationNlamFactor <- getUniform shaderProg "nlamFactor"
    locationMetallic <- getUniform shaderProg "metallic"

    with (V4 1 1 1 1 :: V4 Float) $ \ptr -> do
        glUniform4fv locationBaseColorFactor 1 (castPtr ptr)

    with (V3 1 1 1 :: V3 Float) $ \ptr -> do
        glUniform4fv locationPlaneDir 1 (castPtr ptr)

    return MRShader{..}


vertexShaderSource :: String
vertexShaderSource = [r|
    #version 330
    uniform mat4 projection;
    uniform mat4 model;
    in vec3 in_Position;
    in vec2 in_UV;
    in vec3 in_Normal;
    out vec2 texUV;
    out vec3 normal;
    out vec4 viewPos;
    out vec4 color;
    void main(void) {
        viewPos = model * vec4(in_Position, 1.0);
        gl_Position = projection * viewPos;
        texUV = in_UV;
        normal = mat3(model) * in_Normal;
        color = vec4(1.0, 1.0, 1.0, 1.0);
    }
|]

fragmentShaderSource :: String
fragmentShaderSource = [r|
    #version 330
    #define PI 3.1415926538
    #define dielectricSpecular vec4(0.04, 0.04, 0.04, 1)
    #define black vec4(0, 0, 0, 1)
    out vec4 fragColor;
    in vec2 texUV;
    in vec3 normal;
    in vec4 viewPos;
    in vec4 color;
    in float metallic;
    uniform sampler2D tex;
    uniform vec4 baseColorFactor;
    uniform vec3 planeDir;
    uniform vec3 sight;
    uniform float lamFactor;
    uniform float nlamFactor;
    void main(void){
        vec3 nnorm = normalize(normal);
        vec3 h = normalize(sight + planeDir);

        vec4 baseColor = texture(tex, texUV) * color * baseColorFactor;
        vec4 dfColor = mix(baseColor * (1 - dielectricSpecular[0]), black, lamFactor);
        vec4 spColor = mix(dielectricSpecular, baseColor, lamFactor);

        float lamValue = dot(nnorm, planeDir);
        float spValue = dot(nnorm, h);

        vec4 fresnel = spColor + (vec4(1,1,1,1) - spColor) * pow(1.0 - dot(sight, h), 5);
        fragColor =
            spValue * fresnel
            + lamValue * ((vec4(1,1,1,1) - fresnel) * dfColor) * 0.5
            + 0.5 * baseColor
            ;
        fragColor[3] = 1.0;
    }
|]

registerTextures :: MonadIO m => Sampler -> V2 Int -> [(V2 Int, Jp.Image Jp.PixelRGBA8)] -> m GLuint
registerTextures Sampler{..} (V2 sw sh) imgs = liftIO $ do
    tex <- overPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D tex
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromMaybe GL_LINEAR samplerMinFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromMaybe GL_LINEAR samplerMagFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromMaybe GL_REPEAT samplerWrapS
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromMaybe GL_REPEAT samplerWrapT
    glPixelStorei GL_UNPACK_ALIGNMENT 4
    glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
    glPixelStorei GL_UNPACK_LSB_FIRST 0
    glPixelStorei GL_UNPACK_ROW_LENGTH 0
    glPixelStorei GL_UNPACK_SKIP_IMAGES 0
    glPixelStorei GL_UNPACK_SKIP_PIXELS 0
    glPixelStorei GL_UNPACK_SKIP_ROWS 0
    glPixelStorei GL_UNPACK_SWAP_BYTES 0
    let level = floor $ logBase (2 :: Float) $ fromIntegral (max sw sh)

    glTexStorage2D GL_TEXTURE_2D level GL_SRGB8_ALPHA8 (fromIntegral sw) (fromIntegral sh)

    forM_ imgs $ \(V2 x y, Jp.Image w h vec) -> SV.unsafeWith vec $
        glTexSubImage2D
            GL_TEXTURE_2D 0
            (fromIntegral x)
            (fromIntegral y)
            (fromIntegral w)
            (fromIntegral h)
            GL_RGBA
            GL_UNSIGNED_BYTE
        . castPtr

    glGenerateMipmap GL_TEXTURE_2D

    return tex
