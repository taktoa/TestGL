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
main = withCString "query-objects" $ \cstr -> do
  window <- glInit cstr
  giveContext $ flip evalStateT initialState $ forever $ mainLoop window

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

clearFB = clear clearing { clearDepth = Just 1.0
                         , clearColor = Just $ rgba 0.1 0.1 0.1 1.0 }

type Matrix2D e = [[e]]  -- ^ 2D matrix
type PC = Int            -- ^ Pixel coordinate
type TC = Int            -- ^ Tile map coordinates
type V2 a = (a, a)       -- ^ 2-vector
type V3 a = (a, a, a)    -- ^ 3-vector
type V4 a = (a, a, a, a) -- ^ 4-vector

type PixelCoord  = V2 PC
type PixelDim    = V2 PC
type Pixel       = Graphics.Caramia.Color
type TileDim     = V2 TC
type TileCoord   = V2 TC
type Sprite      = Matrix2D Pixel

type PixelRect   = V2 PixelCoord

class TileMap tm where
  -- | Get a matrix of sprites out of the tilemap
  getTMap :: tm -> Matrix2D Sprite
  getTMap = undefined

  -- | Get the tile coordinate dimensions of the tilemap
  getTMCSize :: tm -> TileDim
  getTMCSize = undefined

  -- | Get the dimensions of each sprite in the tilemap
  getTMSSize :: tm -> PixelDim
  getTMSSize = undefined

  -- | Get the pixel dimensions of the tilemap
  getTMPSize :: tm -> PixelDim
  getTMPSize = undefined

  -- | Verify that a given tilemap is valid
  verifyTileMap :: tm -> Bool
  verifyTileMap = undefined

  -- | Make a TileMap concrete
  renderTMap :: tm -> tm
  renderTMap = undefined

  -- | Index into a TileMap
  indexTMap :: tm -> TileCoord -> Sprite
  indexTMap = undefined

  -- | Make a sprite from a rectangle between two pixel coordinates
  getTMapRect :: tm -> PixelRect -> Sprite
  getTMapRect = undefined

  -- |


mainLoop window = do
  clearFB screenFramebuffer

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

