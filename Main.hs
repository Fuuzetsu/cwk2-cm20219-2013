-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- |
-- Module      :  Main
-- Description :  Coursework 2 submission for CM20219 course ran in
--                University of Bath in 2013. Submission by mk440
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPLv3
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where


import           Control.Applicative
import           Control.Lens
import           Data.IORef
import           Data.Time.Clock
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT
import           System.Exit

-- | Specifies a glVertex3f from a 'GLfloat' triple.
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

-- Here we create field instances for Vertex3 as convenience.
instance Field1 (Vertex3 a) (Vertex3 a) a a where
  _1 k (Vertex3 x y z) = indexed k (0 :: Int) x <&> \x' -> Vertex3 x' y z

instance Field2 (Vertex3 a) (Vertex3 a) a a where
  _2 k (Vertex3 x y z) = indexed k (0 :: Int) y <&> \y' -> Vertex3 x y' z

instance Field3 (Vertex3 a) (Vertex3 a) a a where
  _3 k (Vertex3 x y z) = indexed k (0 :: Int) z <&> \z' -> Vertex3 x y z'


-- | Encoding of the status of three mouse buttons. Each button is
-- in a state specified by 'KeyState'
data MouseState = MouseState { _leftButton   :: KeyState
                             , _rightButton  :: KeyState
                             , _middleButton :: KeyState
                             } deriving (Eq, Show)

-- | The position of the eye camera from the origin.
data CameraRotation = CameraRotation { _cTheta  :: GLdouble
                               , _cPhi    :: GLdouble
                               , _cRadius :: GLdouble
                               }
                      deriving (Eq, Show)

-- | Various program flags. Note that rendering 'Info' or other text
-- results in pretty hefty frame rate drop on my (very slow) netbook so it's
-- not advisable to use this unless it's for debugging.
data Flag = Info -- ^ Display information on the screen
          | ShiftInfo -- ^ Information on current 'CameraRotation'
          | DragInfo -- ^ Show information on mouse drag.
          | MouseInfo -- ^ Information on 'MouseState'
          | FramesPerSecond -- ^ Renders frames per seconds. Ironically
                            -- this causes the FPS to drop.
          | Axis -- ^ X/Y/Z axis drawing. X = Red Y = Green Z = Blue
                 -- Draw from origin to positive <relatively large number>.
          | FocusPoint -- ^ Show the current camera focus point.
          deriving (Show, Eq)

-- | We carry program state using this data type. We do the disgusting thing
-- and wrap it in 'IORef' (or 'MVar'. It's not as evil as it normally would be
-- considering we're running in the IO monad anyway for OpenGL/GLUT but it's not
-- ideal either.
data ProgramState = ProgramState
                      { _cameraRotation :: CameraRotation
                      , _mouseState  :: MouseState
                      , _dragState   :: (MouseState, Maybe Position)
                                      -- ^ The second element of the pair
                                      -- indicates the last position we were at
                                      -- during a mouse drag.
                      , _timeState   :: (DiffTime, Int, Int)
                                      -- ^ Timing information. Last second,
                                      -- number of frames since last second and
                                      -- number of frames in the second
                                      -- beforehand.
                      , _cameraFocus :: ([Vertex3 GLdouble], Int)
                      }

-- We generate lenses with Template Haskell here.
makeLenses ''MouseState
makeLenses ''ProgramState
makeLenses ''CameraRotation


-- | A sensible starting state for the program.
defaultState :: ProgramState
defaultState = ProgramState
  { _cameraRotation = CameraRotation 3.8 6.8 15
  , _mouseState = MouseState Up Up Up
  , _dragState = (MouseState Up Up Up, Nothing)
  , _timeState = (secondsToDiffTime 0, 0, 0)
  , _cameraFocus = (Vertex3 0 0 0 : map tvd (cubeCorners cubeSize), 0)
  }

-- | Same as '($=!)' except with left fixity of 0 for convenience. Clashes with
-- '($)' but that's usually not a problem with lenses.
($$!) :: HasSetter s => s a -> a -> IO ()
($$!) = ($=!)
infixl 0 $$!

-- | Helper to convert a 'CameraRotation' to 'Vertex3' parametrised
-- by 'GLdouble'.
ctvf :: CameraRotation -> Vertex3 GLdouble
ctvf (CameraRotation x y z) = Vertex3 x y z

tvd :: (GLfloat, GLfloat, GLfloat) -> Vertex3 GLdouble
tvd (x, y, z) = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

cubeVertices :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
cubeVertices w =
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

cubeCorners :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
cubeCorners w =
  [ (w, w, w), (-w, w, w), (-w, -w, w), (w, -w, w)
  , (w, w, -w), (-w, w, -w), (-w, -w, -w), (w, -w, -w) ]

-- | Renders a cube with edges of a specified length.
cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f (cubeVertices w)

-- | Renders a frame for a cube with edges of a specified length.
cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]

-- | Update current time in the 'ProgramState' to allow us to count
-- | frames per second rendered.
updateTime :: IORef ProgramState -> IO ()
updateTime ps = do
  programState <- get ps
  t <- utctDayTime <$> getCurrentTime
  let oldTime = programState ^. timeState . _1
  let p' = if t - oldTime >= 1
           then let allFrames = programState ^. timeState . _2
                in programState & timeState .~ (t, 0, allFrames)
           else programState

  ps $$! p' & timeState . _2 %~ succ

preservingColor :: IO a -> IO ()
preservingColor f = get currentColor >>= \c -> f >> color c

-- | Display callback.
display :: IORef ProgramState -> DisplayCallback
display ps = do
  programState <- get ps

  let CameraRotation theta phi radius = programState ^. cameraRotation
      (points, focn)  = programState ^. cameraFocus
      Vertex3 x y z = points !! focn

  clear [ColorBuffer, DepthBuffer]

  loadIdentity
  preservingMatrix (renderInfo programState)

  -- Camera movement
  translate $ Vector3 x y (-z + (-radius))
  rotate ((theta - pi) * (180 / pi)) $ Vector3 1 0 0
  rotate ((-phi) * (180/pi)) $ Vector3 0 1 0

  preservingColor . preservingMatrix $ drawCube True

  updateTime ps

  swapBuffers

-- | Renders information using the current 'ProgramState'
-- For this submission we only render FPS.
renderInfo :: ProgramState -> IO ()
renderInfo p = do
  let fps = (\x -> show x ++ " FPS") (p ^. timeState . _3 )

  c <- get currentColor

  matrixMode $= Projection
  preservingMatrix $ do

    loadIdentity
    Size x y <- get windowSize
    ortho2D 0.0 (fromIntegral x) 0.0 (fromIntegral y)

    matrixMode $= Modelview 0
    preservingMatrix $ do
      color $ Color3 1.0 0.0 (0.0 :: GLfloat)
      rasterPos $ Vertex2 22 (22 :: GLfloat)
      renderString Helvetica18 fps
      color c


-- | Draw a face of a cube. Used by 'drawCube'.
drawFace :: Bool -> GLfloat -> IO ()
drawFace filled s =
  renderPrimitive (if filled then Polygon else LineLoop) $ do
    vertex3f(-s, -s, s)
    vertex3f(s, -s, s)
    vertex3f(s, s, s)
    vertex3f(-s, s, s)

cubeSize :: GLfloat
cubeSize = 0.5

-- | Draws a cube or a cube wireframe dependending on the passed in
-- value.
drawCube :: Bool -- ^ If true, the cube is solid. Else, it's wireframe.
            -> IO ()
drawCube filled = do
  color $ Color3 1.0 0 (0 :: GLdouble)
  drawFace filled cubeSize

  mapM_ (const $ r q 1 0 0 >> drawFace filled cubeSize) [1 .. 3 :: Integer]

  r q 1 0 0
  r q 0 1 0
  drawFace filled cubeSize

  r f 0 1 0
  drawFace filled cubeSize

  where
    f, q :: GLfloat
    f = 180.0
    q = 90.0

    r :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
    r r' x y z = rotate r' $ Vector3 x y z

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]

  _ <- createWindow "Cube viewer"
  ps <- newIORef defaultState
  displayCallback $= display ps
  idleCallback $= Just idle
  keyboardMouseCallback $= Just (keyboardMouse ps)
  reshapeCallback $= Just reshape
  motionCallback $= Just (motion ps)
  depthFunc $= Just Lequal
  matrixMode $= Projection
  perspective 40.0 1.0 1.0 10.0
  matrixMode $= Modelview 0
  initGL
  mainLoop

-- | Reshape callback. Takes in window size.
reshape :: Size -> IO ()
reshape (Size w h) = do
  viewport $= (Position 0 0, Size w h)
  matrixMode $= Projection
  loadIdentity
  frustum (-1) 1 (-1) 1 5 11500
  matrixMode $= Modelview 0

-- | Initialize OpenGL options. Includes light model and material
-- attributes.
initGL :: IO ()
initGL = do
  lighting $= Enabled
  light l $= Enabled

  lightModelTwoSide $= Disabled
  lightModelAmbient $= Color4 1 1 1 1

  materialDiffuse Front $= whiteDir
  materialSpecular Front $= whiteDir
  materialShininess Front $= 200

  diffuse l $= whiteDir
  specular l $= whiteDir
  position l $= Vertex4 30 30 30 1

  shadeModel $= Smooth

  cullFace $= Just Back
  hint PerspectiveCorrection $= Nicest

  initLight

  where
    l = Light 0
    whiteDir = Color4 2 2 2 1

-- | Initialise light (position, diffuse, ambient, …).
initLight :: IO ()
initLight = do
  let mSpec = Color4 1 1 1 1
      sh = 128
      pos = Vertex4 0.2 0.2 0.9 0.0
      amb = Color4 0.05 0.05 0.05 1.0
      lSpec = Color4 0.99 0.99 0.99 1.0
      diff = Color4 0.7 0.7 0.7 1.0

      l = Light 0

  shadeModel $= Smooth

  materialSpecular Front $= mSpec
  materialShininess Front $= sh

  position l $= pos
  specular l $= lSpec
  ambient l $= amb
  diffuse l $= diff

  colorMaterial $= Just (Front, Diffuse)

  lighting $= Enabled
  light l $= Enabled

-- | Idle callback. We just redraw the frame whenever possible.
idle :: IdleCallback
idle = postRedisplay Nothing

-- | A callback for mouse dragging. Deals with offsetting the 'CameraRotation' in
-- 'ProgramState'.
motion :: IORef ProgramState -> MotionCallback
motion ps p@(Position newX newY) = do
  pState <- get ps
  let nowMS = pState ^. mouseState
      (oldMS, oldPos) = pState ^. dragState

  case oldPos of
    Nothing -> ps $$! pState & dragState .~ (nowMS, Just p)
    Just (Position oldX oldY) ->
      if oldMS == nowMS
      then do
        let CameraRotation st sp sr = pState ^. cameraRotation

            dx = fromIntegral $ newX - oldX
            dy = fromIntegral $ newY - oldY

            newTheta = st + dy * (pi / 800)
            newPhi = sp - dx * (pi / 800)


        let p' = case nowMS of
              MouseState Down _ _ -> pState & cameraRotation
                                     .~ CameraRotation newTheta newPhi sr
              MouseState _ Down _ -> pState & cameraRotation
                                     .~ CameraRotation st sp (dy + sr)
              MouseState {} -> pState

        ps $$! p' &  dragState . _2 .~ Just p

      else ps $$! pState & dragState .~ (nowMS, Just p)

-- | Advances camera focus between pre-defined points on the cube. Effectively a
-- poor-man's zipper.
advanceFocus :: IORef ProgramState -> IO ()
advanceFocus ps = do
  p <- get ps
  let (points, n) = p ^. cameraFocus
  ps $$! p & cameraFocus . _2 %~ if n >= (length points - 1)
                                 then const 0
                                 else succ

-- | Keyboard and mouse callback. Deals with flag toggling and camera shifting.
keyboardMouse :: IORef ProgramState -> KeyboardMouseCallback
keyboardMouse ps key Down _ _ = case key of
  Char 'q' -> exitSuccess
  Char 'r' -> get ps >>= \p -> ps $$! p & cameraRotation
                               .~ defaultState ^. cameraRotation
  Char 'n' -> advanceFocus ps

  MouseButton LeftButton -> setB leftButton
  MouseButton RightButton -> setB rightButton
  MouseButton MiddleButton -> advanceFocus ps >> setB middleButton
  MouseButton WheelDown -> get ps >>= \p -> ps $$! p & cameraRotation . cRadius
                                            %~ succ
  _ -> return ()
  where
    setB f = do
      p <- get ps
      ps $$! p & mouseState . f .~ Down & dragState . _2 .~ Nothing

keyboardMouse ps key Up _ _ = case key of
  MouseButton LeftButton -> setB leftButton
  MouseButton RightButton -> setB rightButton
  MouseButton MiddleButton -> setB middleButton
  MouseButton WheelUp -> get ps >>= \p -> ps $$! p & cameraRotation . cRadius
                                          %~ pred
  _ -> return ()
  where
    setB f = get ps >>= \p -> ps $$! p & mouseState . f .~ Up
