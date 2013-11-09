module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import System.Exit

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

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

display :: IORef Bool -> IORef GLfloat
           -> IORef Bool -> DisplayCallback
display lights angle f = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  l <- get lights
  light (Light 0) $= (if' l Enabled Disabled)
  lookAt (Vertex3 5.0 5.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  a <- get angle
  rotate (xangle + a) $ Vector3 1.0 0.0 0.0
  rotate yangle $ Vector3 0.0 1.0 0.0
  rotate zangle $ Vector3 0.0 0.0 1.0
  angle $~! (+ 1)

  drawCube f
  swapBuffers

drawFace :: Bool -> GLfloat -> IO ()
drawFace filled s =
  renderPrimitive (if filled then Polygon else LineLoop) $ do
    vertex3f(-s, -s, s)
    vertex3f(s, -s, s)
    vertex3f(s, s, s)
    vertex3f(-s, s, s)

if' :: Bool -> a -> a -> a
if' p f g = if p then f else g

drawCube :: IORef Bool -> IO ()
drawCube fi = do
  filled <- get fi
  drawFace filled w

  mapM_ (const $ r q 1 0 0 >> drawFace filled w) [1 .. 3]

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
  angle <- newIORef 0.0
  lights <- newIORef True
  fill <- newIORef True
  displayCallback $= display lights angle fill
  idleCallback $= Just idle
  keyboardMouseCallback $= Just (keyboardMouse lights fill)
  reshapeCallback $= Just reshape
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
idle = do
  postRedisplay Nothing


keyboardMouse :: IORef Bool -> IORef Bool -> KeyboardMouseCallback
keyboardMouse lights fill key Down _ _ = case key of
  Char 'q' -> exitSuccess
  Char 'f'-> fill $~! not
  Char 'l' -> lights $~! not
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()
