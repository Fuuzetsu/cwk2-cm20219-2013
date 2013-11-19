{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import System.Exit

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z


type LightState = Bool
type FillState = Bool

data MouseState = MouseState { leftButton :: KeyState
                             , rightButton :: KeyState
                             , middleButton :: KeyState
                             } deriving (Eq, Show)

data CameraRotation = CameraRotation GLdouble GLdouble GLdouble
                      deriving (Eq, Show)


data ProgramState = ProgramState { _cameraRotation :: CameraRotation
                                 , _mouseState :: MouseState
                                 , _lightState :: LightState
                                 , _fillState :: FillState
                                 , _angleState :: GLfloat
                                 , _dragState :: (MouseState, Maybe Position)
                                 , _extraInfo :: [String]
                                 , _radiusState :: GLdouble
                                 }
makeLenses ''ProgramState

defaultState :: ProgramState
defaultState = ProgramState
  { _cameraRotation = CameraRotation 10.0 0.0 0.0
  , _mouseState = MouseState Up Up Up
  , _lightState = True
  , _fillState = True
  , _angleState = 0.0
  , _dragState = (MouseState Up Up Up, Nothing)
  , _extraInfo = []
  , _radiusState = 10.0
  }

ctvf :: CameraRotation -> Vertex3 GLdouble
ctvf (CameraRotation x y z) = Vertex3 x y z

ctvd :: CameraRotation -> Vertex3 GLfloat
ctvd (CameraRotation x y z) = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]

xangle, yangle, zangle :: GLfloat
xangle = 0.0
yangle = 0.0
zangle = 0.0

display :: IORef ProgramState -> DisplayCallback
display ps = do
  programState <- get ps
  let l = view lightState programState
      a = view angleState programState
      f = view fillState programState
      c = view cameraRotation programState
  clear [ColorBuffer, DepthBuffer]

  loadIdentity
--  preservingMatrix (renderInfo programState)
  light (Light 0) $= if' l Enabled Disabled
  lookAt (ctvf c) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  rotate (xangle) $ Vector3 1.0 0.0 0.0
  rotate (yangle) $ Vector3 0.0 1.0 0.0
  rotate (zangle) $ Vector3 0.0 0.0 1.0
  ps $=! programState { _angleState = _angleState programState + 1.0 }

  drawCube f

  swapBuffers

renderInfo :: ProgramState -> IO ()
renderInfo p = do
  let info = [ show $ p ^. mouseState
             , show $ p ^. cameraRotation
             ] ++ p ^. extraInfo

  c <- get currentColor

  matrixMode $= Projection
  preservingMatrix $ do

    loadIdentity
    Size x y <- get windowSize
    ortho2D 0.0 (fromIntegral x) 0.0 (fromIntegral y)

    matrixMode $= Modelview 0
    preservingMatrix $ do
      color $ Color3 1.0 0.0 (0.0 :: GLfloat)
      let positions = [ Vertex2 22 (x :: GLint) | x <- [22, 44 .. ] ]
          r = zip positions (reverse info)
      mapM_ (\(p', t) -> rasterPos p' >> renderString Helvetica18 t) r

  color c

drawFace :: Bool -> GLfloat -> IO ()
drawFace filled s =
  renderPrimitive (if filled then Polygon else LineLoop) $ do
    vertex3f(-s, -s, s)
    vertex3f(s, -s, s)
    vertex3f(s, s, s)
    vertex3f(-s, s, s)

if' :: Bool -> a -> a -> a
if' p f g = if p then f else g

drawCube :: Bool -> IO ()
drawCube filled = do
  drawFace filled w

  mapM_ (const $ r q 1 0 0 >> drawFace filled w) [1 .. 3 :: Integer]

  r q 1 0 0
  r q 0 1 0
  drawFace filled w

  r f 0 1 0
  drawFace filled w

  where
    w = 0.8

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

reshape :: Size -> IO ()
reshape (Size w h) = do
  viewport $= (Position 0 0, Size w h)
  matrixMode $= Projection
  loadIdentity
  frustum (-1) 1 (-1) 1 5 1500
  matrixMode $= Modelview 0


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
  cullFace $= Just Back
  hint PerspectiveCorrection $= Nicest

  initLight

  where
    l = Light 0
    whiteDir = Color4 2 2 2 1

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


idle :: IdleCallback
idle = postRedisplay Nothing

motion :: IORef ProgramState -> MotionCallback
motion ps p@(Position newX newY) = do
  programState <- get ps
  let nowMS = programState ^. mouseState
      (oldMS, oldPos) = programState ^. dragState

  case oldPos of
    Nothing -> ps $=! (dragState .~ (nowMS, Just p) $ programState)
    Just (Position oldX oldY) ->
      if oldMS == nowMS
      then do
        let theta = (fromIntegral $ newX - oldX) * 0.005
            phi = (fromIntegral $ newY - oldY) * 0.005
            radius = programState ^. radiusState
            eyeX = -(radius * (cos phi) * (sin theta))
            eyeY = -(radius * sin phi * sin theta)
            eyeZ = radius * cos theta
            CameraRotation xc yc zc = programState ^. cameraRotation
            newCR = case programState ^. mouseState of
              MouseState Down _ _ -> CameraRotation eyeX eyeY eyeZ
      --        MouseState _ Down _ -> CameraRotation xc (yc + 2) zc
              MouseState _ _ _ -> programState ^. cameraRotation

        ps $=! (dragState .~ (nowMS, Just p) $ programState)
        ps $=! (cameraRotation .~ newCR $ programState)


      else ps $=! (dragState .~ (nowMS, Just p) $ programState)


keyboardMouse :: IORef ProgramState -> KeyboardMouseCallback
keyboardMouse ps key Down _ _ = case key of
  Char 'q' -> exitSuccess
  Char 'f'-> get ps >>= \p -> ps $=! p { _fillState = not $ _fillState  p }
  Char 'l' -> get ps >>= \p -> ps $=! p { _lightState = not $ _lightState p }
  Char 'r' -> get ps >>= \p -> ps $=! p { _cameraRotation = CameraRotation 0 0 0 }
  Char 'x' -> do
    p <- get ps
    let CameraRotation x y z = p ^. cameraRotation
    ps $=! p { _cameraRotation = CameraRotation (x + 1) y z }
  Char 'y' -> do
    p <- get ps
    let CameraRotation x y z = p ^. cameraRotation
    ps $=! p { _cameraRotation = CameraRotation (x) (y + 1) z }
  Char 'z' -> do
    p <- get ps
    let CameraRotation x y z = p ^. cameraRotation
    ps $=! p { _cameraRotation = CameraRotation x y (z + 1) }
  MouseButton LeftButton -> setLeftButton ps Down >> resetDrag ps
  MouseButton RightButton -> setRightButton ps Down >> resetDrag ps
  MouseButton MiddleButton -> setMiddleButton ps Down >> resetDrag ps
  MouseButton WheelDown -> get ps >>= \p -> ps $=! p { _radiusState = _radiusState p - 1.0 }
  _ -> print "Mouse down"

keyboardMouse ps key Up _ _ = case key of
  MouseButton LeftButton -> setLeftButton ps Up
  MouseButton RightButton -> setRightButton ps Up
  MouseButton MiddleButton -> setMiddleButton ps Up
  MouseButton WheelUp -> get ps >>= \p -> ps $=! p { _radiusState = _radiusState p + 1.0 }
  _ -> print "Mouse up"

resetDrag :: IORef ProgramState -> IO ()
resetDrag ps = do
  p <- get ps
  let (m, _) = _dragState p
  ps $=! p { _dragState = (m, Nothing) }

setLeftButton :: IORef ProgramState -> KeyState -> IO ()
setLeftButton ps s = do
  p <- get ps
  let MouseState _ right mid = p ^. mouseState
  ps $=! p { _mouseState = MouseState s right mid }

setRightButton :: IORef ProgramState -> KeyState -> IO ()
setRightButton ps s = do
  p <- get ps
  let MouseState left _ mid = p ^. mouseState
  ps $=! p { _mouseState = MouseState left s mid }

setMiddleButton :: IORef ProgramState -> KeyState -> IO ()
setMiddleButton ps s = do
  p <- get ps
  let MouseState left right _ = p ^. mouseState
  ps $=! p { _mouseState = MouseState left right s }
