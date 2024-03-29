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

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where


import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Time.Clock
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import System.Exit

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
data MouseState = MouseState { _leftButton :: KeyState
                             , _rightButton :: KeyState
                             , _middleButton :: KeyState
                             } deriving (Eq, Show)

-- | The position of the eye camera from the origin.
data CameraShift = CameraShift { _cTheta :: GLdouble
                               , _cPhi :: GLdouble
                               , _cRadius :: GLdouble
                               }
                      deriving (Eq, Show)

-- | Various program flags. Note that rendering 'Info' or other text
-- results in pretty hefty frame rate drop on my (very slow) netbook so it's
-- not advisable to use this unless it's for debugging.
data Flag = Info -- ^ Display information on the screen
          | ExtraInfo -- ^ Printing of extra information produced
                      -- throughout the program.
          | Flags -- ^ Show enabled flags in the information.
          | ShiftInfo -- ^ Information on current 'CameraShift'
          | Middle -- ^ Translate to the middle of many shapes. Only useful
                   -- with 'dl'. Currently broken
          | Shapes -- ^ Render a lot more shapes. For fun.
          | DragInfo -- ^ Show information on mouse drag.
          | MouseInfo -- ^ Information on 'MouseState'
          | Fill -- ^ Fill the shapes we are rendering.
          | Lights -- ^ Turns on lighting.
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
                      { _cameraShift :: CameraShift
                      , _mouseState :: MouseState
                      , _angleState :: GLfloat
                      , _dragState :: (MouseState, Maybe Position)
                                      -- ^ The second element of the pair
                                      -- indicates the last position we were at
                                      -- during a mouse drag.
                      , _extraInfo :: [String]
                      , _flags :: [Flag]
                      , _timeState :: (DiffTime, Int, Int)
                                      -- ^ Timing information. Last second,
                                      -- number of frames since last second and
                                      -- number of frames in the second
                                      -- beforehand.
                      , _cameraFocus :: ([Vertex3 GLdouble], Int)
                      }

-- We generate lenses with Template Haskell here.
makeLenses ''MouseState
makeLenses ''ProgramState
makeLenses ''CameraShift

-- | Enable a 'Flag' for a given 'ProgramState'.
addFlag :: IORef ProgramState -> Flag -> IO ()
addFlag ps f = do
  p <- get ps
  ps $$! p & flags %~ (\fl -> if f `elem` fl then fl else f : fl)

-- | Clear a 'Flag' from the 'ProgramState'
clearFlag :: IORef ProgramState -> Flag -> IO ()
clearFlag ps f = get ps >>= \p -> ps $$! p & flags %~ filter (/= f)

-- | Toggles a 'Flag' in a 'ProgramState'
toggleFlag :: IORef ProgramState -> Flag -> IO ()
toggleFlag ps f = do
  p <- get ps
  if f `elem` p ^. flags then clearFlag ps f else addFlag ps f

-- | Helper for '_extraInfo' that will only keep a preset amount of
-- logging information.
(++?) :: [a] -> [a] -> [a]
x ++? y
  | length x >= 20 = tail x ++ y
  | otherwise = x ++ y

-- | A sensible starting state for the program.
defaultState :: ProgramState
defaultState = ProgramState
  { _cameraShift = CameraShift 3.8 6.8 15
  , _mouseState = MouseState Up Up Up
  , _angleState = 0.0
  , _dragState = (MouseState Up Up Up, Nothing)
  , _extraInfo = []
  , _flags = [ Flags, Lights, FramesPerSecond
             , Main.Fill, DragInfo, MouseInfo, ShiftInfo
             , Shapes, Axis, FocusPoint
             ]
  , _timeState = (secondsToDiffTime 0, 0, 0)
  , _cameraFocus = (Vertex3 0 0 0 : map tvd (cubeCorners cubeSize), 0)
  }

-- | Same as '($=!)' except with left fixity of 0 for convenience. Clashes with
-- '($)' but that's usually not a problem with lenses.
($$!) :: HasSetter s => s a -> a -> IO ()
($$!) = ($=!)
infixl 0 $$!

-- | Helper to convert a 'CameraShift' to 'Vertex3' parametrised
-- by 'GLdouble'.
ctvf :: CameraShift -> Vertex3 GLdouble
ctvf (CameraShift x y z) = Vertex3 x y z

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

  let l = Lights `elem` programState ^. flags
      f = Main.Fill `elem` programState ^. flags
      CameraShift theta phi radius = programState ^. cameraShift
      (points, focn)  = programState ^. cameraFocus
      Vertex3 x y z = points !! focn

  clear [ColorBuffer, DepthBuffer]

  when (Axis `elem` programState ^. flags)
    (preservingMatrix drawAxis >> return ())

  loadIdentity
  preservingMatrix (renderInfo programState)
  lighting $= if' l Enabled Disabled

  -- Camera movement
  let [nx, ny, nz] = [ radius * cos theta * sin phi
                     , radius * sin theta * sin phi
                     , radius * cos phi
                     ]

  translate $ Vector3 x y (-z + (-radius))
  rotate ((theta - pi) * (180 / pi)) $ Vector3 1 0 0
  rotate ((-phi) * (180/pi)) $ Vector3 0 1 0

  preservingColor . preservingMatrix $ do
    drawCube f

  ps $$! programState & angleState %~ (+ 1.0)
  writeLog ps [nx, ny, nz]
  updateTime ps

  swapBuffers

-- | Draws the X, Y and Z axis from origin towards positive <relatively
-- large number>. X axis is red, Y axis is green and Z axis is green.
-- Additionally, horizontal spacers are drawn every 1 unit on each axis.
drawAxis :: IO DisplayList
drawAxis = do

  defineNewList CompileAndExecute $ do
    c <- get currentColor
    let len = 500.0
        step = 1
        apply3 f (x, y, z) = (f x, f y, f z)
        xv = (1.0, 0, 0)
        yv = (0.0, 1.0, 0)
        zv = (0.0, 0.0, 1.0)
        rl = [step , step + step .. len]
    preservingMatrix $ do
      -- X
      color $ Color3 1.0 0.0 (0.0 :: GLfloat)

      renderPrimitive Lines $ mapM_ vertex3f $
        [ (0.0, 0.0, 0.0)
        , apply3 (* len) xv
        ] ++ concat (map (\x -> [(x, 0, -1) , (x, 0, 1)] ) rl)

      -- Y
      color $ Color3 0.0 1.0 (0.0 :: GLfloat)

      renderPrimitive Lines $ mapM_ vertex3f $
        [ (0.0, 0.0, 0.0)
        , apply3 (* len) yv
        ] ++ concat (map (\x -> [(-1, x, 0) , (1, x, 0)] ) rl)
          ++ concat (map (\x -> [(0, x, -1) , (0, x, 1)] ) rl)

      -- Z
      color $ Color3 0.0 0.0 (1.0 :: GLfloat)

      renderPrimitive Lines $ mapM_ vertex3f $
        [ (0.0, 0.0, 0.0)
        , apply3 (* len) zv
        ] ++ concat (map (\x -> [(-1, 0, x) , (1, 0, x)] ) rl)

    color c

-- | Renders information using the current 'ProgramState'
renderInfo :: ProgramState -> IO ()
renderInfo p = do
  let h f g = if f `elem` p ^. flags then [p ^. g ^. to show] else []
      info = if Main.Info `elem` p ^. flags
             then (if ExtraInfo `elem` p ^. flags then p ^. extraInfo else [])
                  ++ h MouseInfo mouseState ++ h DragInfo dragState
                  ++ h ShiftInfo cameraShift
                  ++ if FocusPoint `elem` p ^. flags
                     then let n = p ^. cameraFocus . _2
                          in [show $ (p ^. cameraFocus . _1) !! n]
                     else []
                  ++ h Flags flags
             else []
      fps = if FramesPerSecond `elem` p ^. flags
            then map (++ " FPS") (h FramesPerSecond (timeState . _3))
            else []

  c <- get currentColor

  matrixMode $= Projection
  preservingMatrix $ do

    loadIdentity
    Size x y <- get windowSize
    ortho2D 0.0 (fromIntegral x) 0.0 (fromIntegral y)

    matrixMode $= Modelview 0
    preservingMatrix $ do
      color $ Color3 1.0 0.0 (0.0 :: GLfloat)
      let positions = [ Vertex2 22 (x' :: GLint) | x' <- [22, 44 .. ] ]
          r = zip positions . reverse $
              if FramesPerSecond `elem` p ^. flags
              then info ++ fps
              else info
      mapM_ (\(p', t) -> rasterPos p' >> renderString Helvetica18 t) r
      color c


-- | Draw a face of a cube. Used by 'drawCube'.
drawFace :: Bool -> GLfloat -> IO ()
drawFace filled s =
  renderPrimitive (if filled then Polygon else LineLoop) $ do
    vertex3f(-s, -s, s)
    vertex3f(s, -s, s)
    vertex3f(s, s, s)
    vertex3f(-s, s, s)

-- | Traditional if_then_else as a function.
if' :: Bool -> a -> a -> a
if' p f g = if p then f else g

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

  _ <- createWindow "Spinning cube"
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

  clearColor $= Color4 0.5 1.0 0.75 0.0
  -- clearColor $= Color4 0.0 0.0 0.0 0.0
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

      l :: Light
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

-- | Add a line to '_extraInfo' in the program state.
writeLog :: Show a => IORef ProgramState -> a -> IO ()
writeLog ps s = get ps >>= \p -> ps $$! p & extraInfo %~ (++? [show s])

-- | Clears the '_extraInfo' log.
clearLog :: IORef ProgramState -> IO ()
clearLog ps = get ps >>= \p -> ps $$! p & extraInfo .~ []

-- | Idle callback. We just redraw the frame whenever possible.
idle :: IdleCallback
idle = postRedisplay Nothing

-- | A callback for mouse dragging. Deals with offsetting the 'CameraShift' in
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
        let CameraShift st sp sr = pState ^. cameraShift

            zDifference = fromIntegral (newY - oldY) + sr

            dx = fromIntegral $ newX - oldX
            dy = fromIntegral $ newY - oldY

            newTheta = st + dy * (pi / 800)
            newPhi = sp - dx * (pi / 800)


        let p' = case nowMS of
              MouseState Down _ _ -> pState & cameraShift
                                     .~ CameraShift newTheta newPhi sr
              MouseState _ Down _ -> pState & cameraShift
                                     .~ CameraShift st sp zDifference
              MouseState _ _ _ -> pState


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
                                 else (+ 1)

-- | Keyboard and mouse callback. Deals with flag toggling and camera shifting.
keyboardMouse :: IORef ProgramState -> KeyboardMouseCallback
keyboardMouse ps key Down _ _ = case key of
  Char 'q' -> exitSuccess
  Char 'f'-> toggleFlag ps Main.Fill
  Char 'r' -> get ps >>= \p -> ps $$! p & cameraShift
                               .~ defaultState ^. cameraShift
  Char 'x' -> onCoord cTheta (+ (pi / 200))
  Char 'y' -> onCoord cPhi (+ (pi / 200))
  Char 'z' -> onCoord cRadius succ
  Char 'X' -> onCoord cTheta (flip (-) (pi / 200))
  Char 'Y' -> onCoord cPhi (flip (-) (pi / 200))
  Char 'Z' -> onCoord cRadius pred
  Char 'l' -> toggleFlag ps Lights
  Char 's' -> toggleFlag ps Flags
  Char 'i' -> toggleFlag ps Main.Info
  Char 'e' -> toggleFlag ps ExtraInfo
  Char 'd' -> toggleFlag ps DragInfo
  Char 'm' -> toggleFlag ps MouseInfo
  Char 't' -> toggleFlag ps ShiftInfo
  Char 'p' -> toggleFlag ps FramesPerSecond
  Char 'M' -> toggleFlag ps Middle
  Char 'S' -> toggleFlag ps Shapes
  Char 'a' -> toggleFlag ps Axis
  Char 'n' -> advanceFocus ps

  MouseButton LeftButton -> setB leftButton
  MouseButton RightButton -> setB rightButton
  MouseButton MiddleButton -> setB middleButton
  MouseButton WheelDown -> get ps >>= \p -> ps $$! p & cameraShift . cRadius
                                            %~ succ
  _ -> return ()
  where
    setB f = do
      p <- get ps
      ps $$! p & mouseState . f .~ Down & dragState . _2 .~ Nothing

    onCoord f g = get ps >>= \p -> ps $$! p & cameraShift . f %~ g

keyboardMouse ps key Up _ _ = case key of
  MouseButton LeftButton -> setB leftButton
  MouseButton RightButton -> setB rightButton
  MouseButton MiddleButton -> setB middleButton
  MouseButton WheelUp -> get ps >>= \p -> ps $$! p & cameraShift . cRadius
                                          %~ pred
  _ -> return ()
  where
    setB f = get ps >>= \p -> ps $$! p & mouseState . f .~ Up
