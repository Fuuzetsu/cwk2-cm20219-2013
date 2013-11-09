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

display :: IORef Bool -> DisplayCallback
display f = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  lookAt (Vertex3 5.0 5.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  -- a <- get angle
  rotate xangle $ Vector3 1.0 0.0 0.0
  rotate yangle $ Vector3 0.0 1.0 0.0
  rotate zangle $ Vector3 0.0 0.0 1.0
  -- color $ Color3 (1::GLfloat) 1 1 -- set the cube colour to white
  -- cube 0.5
  -- color $ Color3 (1::GLfloat) 0 0 -- set the outline colour to red
  -- cubeFrame 0.5
  drawDice f
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

drawDice :: IORef Bool -> IO ()
drawDice fi = do
  filled <- get fi
  if' filled cube cubeFrame $ w
  -- drawFace filled w

  -- r f 1 0 0
  -- drawFace filled w

  -- r (-q) 0 1 0
  -- drawFace filled w

  -- r q 0 1 0
  -- drawFace filled w

  -- r q 1 0 0
  -- drawFace filled w

  -- r (-q) 1 0 0
  -- drawFace filled w

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
  -- angle <- newIORef 0.0
  fill <-newIORef False
  displayCallback $= display fill
  idleCallback $= Just idle
  keyboardMouseCallback $= Just (keyboardMouse fill)
  reshapeCallback $= Just reshape
  depthFunc $= Just Lequal
  matrixMode $= Projection
  perspective 40.0 1.0 1.0 10.0
  matrixMode $= Modelview 0
  clearColor $= Color4 0.5 1.0 0.75 0.0
  cullFace $= Nothing
  hint PerspectiveCorrection $= Nicest
  mainLoop

reshape :: Size -> IO ()
reshape (Size w h) = do
  viewport $= (Position 0 0, Size w h)
  matrixMode $= Projection
  loadIdentity
  frustum (-1) 1 (-1) 1 5 1500
  matrixMode $= Modelview 0

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


keyboardMouse :: IORef Bool -> KeyboardMouseCallback
keyboardMouse fill key Down _ _ = case key of
  Char 'q' -> exitSuccess
  Char 'f'-> fill $~! not
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
