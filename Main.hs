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

data MouseState = MouseState { _leftButton :: KeyState
                             , _rightButton :: KeyState
                             , _middleButton :: KeyState
                             } deriving (Eq, Show)

data CameraPosition = CameraPosition { _cX :: GLdouble
                                     , _cY :: GLdouble
                                     , _cZ :: GLdouble
                                     }
                      deriving (Eq, Show)

data Flag = Info | ExtraInfo | CameraInfo | Flags
          | DragInfo | MouseInfo | Fill | Help
          deriving (Show, Eq)

data ProgramState = ProgramState { _cameraPosition :: CameraPosition
                                 , _mouseState :: MouseState
                                 , _lightState :: LightState
                                 , _angleState :: GLfloat
                                 , _dragState :: (MouseState, Maybe Position)
                                 , _extraInfo :: [String]
                                 , _flags :: [Flag]
                                 }
makeLenses ''MouseState
makeLenses ''ProgramState
makeLenses ''CameraPosition

addFlag :: IORef ProgramState -> Flag -> IO ()
addFlag ps f = do
  p <- get ps
  ps $$! p & flags %~ (\fl -> if f `elem` fl then fl else f : fl)

clearFlag :: IORef ProgramState -> Flag -> IO ()
clearFlag ps f = get ps >>= \p -> ps $$! p & flags %~ filter (/= f)

toggleFlag :: IORef ProgramState -> Flag -> IO ()
toggleFlag ps f = do
  p <- get ps
  if f `elem` p ^. flags then clearFlag ps f else addFlag ps f

(++?) :: [a] -> [a] -> [a]
x ++? y
  | length x >= 20 = tail x ++ y
  | otherwise = x ++ y

defaultState :: ProgramState
defaultState = ProgramState
  { _cameraPosition = CameraPosition 0.0 0.0 10.0
  , _mouseState = MouseState Up Up Up
  , _lightState = True
  , _angleState = 0.0
  , _dragState = (MouseState Up Up Up, Nothing)
  , _extraInfo = []
  , _flags = [ Main.Info, ExtraInfo, CameraInfo, Flags
             , Main.Fill, DragInfo, MouseInfo
             ]
  }

-- Remind $=! with to with 0 left fixity to drop some parens
($$!) :: HasSetter s => s a -> a -> IO ()
($$!) = ($=!)
infixl 0 $$!


ctvf :: CameraPosition -> Vertex3 GLdouble
ctvf (CameraPosition x y z) = Vertex3 x y z

ctvd :: CameraPosition -> Vertex3 GLfloat
ctvd (CameraPosition x y z) = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

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
  let l = programState ^. lightState
      a = programState ^. angleState
      f = Main.Fill `elem` programState ^. flags
      c = programState ^. cameraPosition
      Vertex3 x y z = ctvd $ c
  clear [ColorBuffer, DepthBuffer]

  preservingMatrix drawAxis

  loadIdentity
  preservingMatrix (renderInfo programState)
  light (Light 0) $= if' l Enabled Disabled
  lookAt (ctvf c) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  -- rotate xangle $ Vector3 1.0 0.0 0.0
  -- rotate yangle $ Vector3 0.0 1.0 0.0
  -- rotate zangle $ Vector3 0.0 0.0 1.0
  -- ps $$! programState & angleState %~ (+ 1.0)

  preservingMatrix (drawCube f)

  swapBuffers

drawAxis :: IO ()
drawAxis = do
  c <- get currentColor
  preservingMatrix $ do
    -- X
    color $ Color3 1.0 0.0 (0.0 :: GLfloat)

    renderPrimitive Lines $ mapM_ vertex3f
      [ (0.0, 0.0, 0.0)
      , (10.0, 0.0, 0.0)
      ]

    -- Y
    color $ Color3 0.0 1.0 (0.0 :: GLfloat)

    renderPrimitive Lines $ mapM_ vertex3f
      [ (0.0, 0.0, 0.0)
      , (0.0, 10.0, 0.0)
      ]

    -- Z
    color $ Color3 0.0 0.0 (1.0 :: GLfloat)

    renderPrimitive Lines $ mapM_ vertex3f
      [ (0.0, 0.0, 0.0)
      , (0.0, 0.0, 10.0)
      ]

  color c

renderInfo :: ProgramState -> IO ()
renderInfo p = do

  let helpText =
        [ "Char 'q' -> exitSuccess"
        , "Char 'f'-> toggleFlag ps Main.Fill"
        , "Char 'l' -> get ps >>= \\p -> ps $$! p & lightState %~ not"
        , "Char 'r' -> get ps >>= \\p -> ps $$! p & cameraPosition .~ defaultState ^. cameraPosition"
        , "Char 'x' -> onCoord cX succ"
        , "Char 'y' -> onCoord cY succ"
        , "Char 'z' -> onCoord cZ succ"
        , "Char 'X' -> onCoord cX pred"
        , "Char 'Y' -> onCoord cY pred"
        , "Char 'Z' -> onCoord cZ pred"
        , "Char 's' -> toggleFlag ps Flags"
        , "Char 'i' -> toggleFlag ps Main.Info"
        , "Char 'c' -> toggleFlag ps CameraInfo"
        , "Char 'e' -> toggleFlag ps ExtraInfo"
        , "Char 'd' -> toggleFlag ps DragInfo"
        , "Char 'm' -> toggleFlag ps MouseInfo"
        , "Char 'h' -> toggleFlag ps Main.Help"
        ]


  let h f g = if f `elem` p ^. flags then [p ^. g ^. to show] else []
      info = (if ExtraInfo `elem` p ^. flags then p ^. extraInfo else [])
             ++ h MouseInfo mouseState ++ h CameraInfo cameraPosition
             ++ h DragInfo dragState ++ h Flags flags
      info' = if Main.Help `elem` p ^. flags then helpText else info

  if Main.Info `elem` p ^. flags
    then do
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
              r = zip positions (reverse info')
          mapM_ (\(p', t) -> rasterPos p' >> renderString Helvetica18 t) r

      color c
    else return ()

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

  -- clearColor $= Color4 0.5 1.0 0.75 0.0
  clearColor $= Color4 0.0 0.0 0.0 0.0
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


writeLog :: Show a => IORef ProgramState -> a -> IO ()
writeLog ps s = get ps >>= \p -> ps $$! p & extraInfo %~ (++? [show s])

clearLog :: IORef ProgramState -> IO ()
clearLog ps = get ps >>= \p -> ps $$! p & extraInfo .~ []

idle :: IdleCallback
idle = postRedisplay Nothing

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
        let xCh = fromIntegral $ newX - oldX

            theta = (fromIntegral $ newX - oldX) * 0.005
            phi = (fromIntegral $ newY - oldY) * 0.005

            radius = defaultState ^. cameraPosition . cZ

            eyeX = -(radius * (cos phi) * (sin theta))
            eyeY = -(radius * sin phi * sin theta)
            eyeZ = radius * cos theta
            CameraPosition xc yc zc = pState ^. cameraPosition
            newCR = case pState ^. mouseState of
              -- MouseState Down _ _ -> CameraPosition (xc + xCh) yc zc
              MouseState Down _ _ -> CameraPosition eyeX eyeY zc
      --        MouseState _ Down _ -> CameraPosition xc (yc + 2) zc
              MouseState _ _ _ -> pState ^. cameraPosition

        ps $$! pState & dragState .~ (nowMS, Just p) & cameraPosition .~ newCR
        writeLog ps p

      else do ps $$! pState & dragState .~ (nowMS, Just p)
              writeLog ps p


keyboardMouse :: IORef ProgramState -> KeyboardMouseCallback
keyboardMouse ps key Down _ _ = case key of
  Char 'q' -> exitSuccess
  Char 'f'-> toggleFlag ps Main.Fill
  Char 'l' -> get ps >>= \p -> ps $$! p & lightState %~ not
  Char 'r' -> get ps >>= \p -> ps $$! p & cameraPosition .~ defaultState ^. cameraPosition
  Char 'x' -> onCoord cX succ
  Char 'y' -> onCoord cY succ
  Char 'z' -> onCoord cZ succ
  Char 'X' -> onCoord cX pred
  Char 'Y' -> onCoord cY pred
  Char 'Z' -> onCoord cZ pred
  Char 's' -> toggleFlag ps Flags
  Char 'i' -> toggleFlag ps Main.Info
  Char 'c' -> toggleFlag ps CameraInfo
  Char 'e' -> toggleFlag ps ExtraInfo
  Char 'd' -> toggleFlag ps DragInfo
  Char 'm' -> toggleFlag ps MouseInfo
  Char 'h' -> toggleFlag ps Main.Help

  MouseButton LeftButton -> setB leftButton
  MouseButton RightButton -> setB rightButton
  MouseButton MiddleButton -> setB middleButton
--  MouseButton WheelDown -> get ps >>= \p -> ps $$! p & cameraPosition %~ flip camMap succ
  _ -> print "Mouse down"
  where
    setB f = do
      p <- get ps
      ps $$! p & mouseState . f .~ Down & dragState . _2 .~ Nothing
      clearLog ps

    onCoord f g = get ps >>= \p -> ps $$! p & cameraPosition . f %~ g

keyboardMouse ps key Up _ _ = case key of
  MouseButton LeftButton -> setB leftButton >> clearLog ps
  MouseButton RightButton -> setB rightButton
  MouseButton MiddleButton -> setB middleButton
--  MouseButton WheelUp -> get ps >>= \p -> ps $$! p & cameraPosition %~ flip camMap pred
  _ -> print "Mouse up"
  where
    setB f = get ps >>= \p -> ps $$! p & mouseState . f .~ Up
