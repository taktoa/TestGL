{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main library for screwing around with OpenGL
module TestGL (module TestGL) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict
import           Data.Bits
import           Data.Text                        (Text)
import qualified Data.Vector.Storable             as V
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Caramia
import           Graphics.Caramia.Prelude         hiding (init)
import           Graphics.UI.SDL

-- GENERATE: import New.Module as TestGL

--
winSizeX, winSizeY :: Num a => a
winSizeX = 500
winSizeY = 500

-- | OpenGL initialization routine
glInit :: MonadIO m => CString -> m Window
glInit cstr = do
  void $ init SDL_INIT_VIDEO
  _ <- glSetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 3
  _ <- glSetAttribute SDL_GL_CONTEXT_MINOR_VERSION 3
  _ <- glSetAttribute SDL_GL_CONTEXT_PROFILE_MASK SDL_GL_CONTEXT_PROFILE_CORE
  _ <- glSetAttribute SDL_GL_CONTEXT_FLAGS SDL_GL_CONTEXT_DEBUG_FLAG
  window <- createWindow cstr undef    undef
                              winSizeX winSizeY
                              props
  _ <- glCreateContext window
  return window
  where
    undef = SDL_WINDOWPOS_UNDEFINED
    props = SDL_WINDOW_OPENGL .|. SDL_WINDOW_SHOWN

-- | Main function
main :: IO ()
main =
    withCString "query-objects" $ \cstr -> do
        window <- glInit cstr
        giveContext $ do
            color_program <- newPipelineVF passThroughVertex2DShader
                                           coloredFragmentProgram
                                           mempty
            color_loc <- getUniformLocation "color" color_program
            offset_loc <- getUniformLocation "offset" color_program
            vao <- newVAO
            pos_vec <- newImmutableBufferFromVector $ V.fromList
                            [ -0.3, -0.3, 0.0
                            ,  0.3, -0.3, 0.0
                            , -0.3,  0.3, 0.0
                            ,  0.3,  0.3, 0.0 ]
            sourceVertexData pos_vec
                             defaultSourcing {
                                 components = 3
                               , attributeIndex = 0
                               , sourceType = SFloat
                             }
                             vao

            flip evalStateT initialState $ forever $
              mainLoop window vao color_program color_loc offset_loc

type PgmState = (Bool, Float)

initialState :: PgmState
initialState = (False, -1.0)

updateState :: PgmState -> PgmState
updateState (x, y)
  | y < -1.0  = (True,  incr y)
  | y > 1.0   = (False, decr y)
  | x         = (x,     incr y)
  | otherwise = (x,     decr y)
  where
    incr a = a + 0.003
    decr a = a - 0.003

mainLoop window vao color_program color_loc offset_loc = do
  clear clearing { clearDepth = Just 1.0
                 , clearColor = Just $ rgba 0.1 0.1 0.1 1.0 }
        screenFramebuffer

  let drawParams = defaultDrawParams { pipeline = color_program
                                     , fragmentPassTests =
                                       defaultFragmentPassTests {
                                         depthTest = Just Less } }

  runDraws drawParams $ draws vao color_program color_loc offset_loc

  modify updateState

  liftIO $ glSwapWindow window
  runPendingFinalizers

draws vao color_program color_loc offset_loc = do
  samples     <- newNumericQuery SamplesPassed
  any_samples <- newBooleanQuery AnySamplesPassed
  elapsed     <- newNumericQuery TimeElapsed
  go_right    <- lift $ fst <$> get
  x           <- lift $ snd <$> get
  setUniform (rgba 1 0 1 1)    color_loc  color_program
  setUniform (x, 0.0 :: Float) offset_loc color_program
  drawR drawCommand {
      primitiveType = TriangleStrip
    , primitivesVAO = vao
    , numIndices = 4
    , sourceData = Primitives { firstIndex = 0 } }
  if go_right then beginQuery samples else beginQuery any_samples
  beginQuery elapsed
  setUniform (0 :: Float, 0 :: Float) offset_loc color_program
  setUniform (rgba 0 1 1 1) color_loc color_program
  if go_right then endQuery samples   else endQuery any_samples
  endQuery elapsed

newImmutableBufferFromVector :: V.Vector Float -> IO Buffer
newImmutableBufferFromVector vec =
    V.unsafeWith vec $ \ptr -> do
        let byte_size = sizeOf (undefined :: Float) * V.length vec
        newBuffer defaultBufferCreation {
                 size = byte_size
               , initialData = Just $ castPtr ptr
               , accessHints = (Static, Draw)
               , accessFlags = NoAccess }

passThroughVertex2DShader :: Text
passThroughVertex2DShader =
    "#version 330\n" <>
    "uniform vec2 offset;\n" <>
    "layout (location=0) in vec2 pos;\n" <>
    "void main() {\n" <>
    "    gl_Position = vec4(pos+offset, 0, 1);\n" <>
    "}\n"

coloredFragmentProgram :: Text
coloredFragmentProgram =
    "#version 330\n" <>
    "uniform vec4 color;\n" <>
    "layout (location=0) out vec4 fcolor;\n" <>
    "void main() {\n" <>
    "    fcolor = color;\n" <>
    "}\n"

