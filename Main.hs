{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module Main where


import Control.Lens
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Fixed
import Data.IORef
import System.Exit

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

instance Field1 (Vertex3 a) (Vertex3 a) a a where
  _1 k (Vertex3 x y z) = indexed k (0 :: Int) x <&> \x' -> Vertex3 x' y z

instance Field2 (Vertex3 a) (Vertex3 a) a a where
  _2 k (Vertex3 x y z) = indexed k (0 :: Int) y <&> \y' -> Vertex3 x y' z

instance Field3 (Vertex3 a) (Vertex3 a) a a where
  _3 k (Vertex3 x y z) = indexed k (0 :: Int) z <&> \z' -> Vertex3 x y z'


type LightState = Bool
type FillState = Bool

data MouseState = MouseState { _leftButton :: KeyState
                             , _rightButton :: KeyState
                             , _middleButton :: KeyState
                             } deriving (Eq, Show)

data CameraShift = CameraShift { _cX :: GLint
                               , _cY :: GLint
                               , _cZ :: GLint
                               }
                      deriving (Eq, Show)

data Flag = Info | ExtraInfo | CameraInfo | Flags | ShiftInfo
          | DragInfo | MouseInfo | Fill | Help
          deriving (Show, Eq)

data ProgramState = ProgramState { _cameraShift :: CameraShift
                                 , _mouseState :: MouseState
                                 , _lightState :: LightState
                                 , _angleState :: GLfloat
                                 , _dragState :: (MouseState, Maybe Position)
                                 , _extraInfo :: [String]
                                 , _flags :: [Flag]
                                 }
makeLenses ''MouseState
makeLenses ''ProgramState
makeLenses ''CameraShift

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
  { _cameraShift = CameraShift 0 0 10
  , _mouseState = MouseState Up Up Up
  , _lightState = True
  , _angleState = 0.0
  , _dragState = (MouseState Up Up Up, Nothing)
  , _extraInfo = []
  , _flags = [ Main.Info, ExtraInfo, Flags
             , Main.Fill, DragInfo, MouseInfo, ShiftInfo
             ]
  }

-- Remind $=! with to with 0 left fixity to drop some parens
($$!) :: HasSetter s => s a -> a -> IO ()
($$!) = ($=!)
infixl 0 $$!


ctvf :: CameraShift -> Vertex3 GLdouble
ctvf (CameraShift x y z) = Vertex3 (fromIntegral x) (fromIntegral y) (fromIntegral z)

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
      c = programState ^. cameraShift
      CameraShift x' y' z' = programState ^. cameraShift
      (x, y, z) = (fromIntegral x' / 10, fromIntegral y' / 10, fromIntegral z')
  clear [ColorBuffer, DepthBuffer]

  preservingMatrix drawAxis

  loadIdentity
  preservingMatrix (renderInfo programState)
  light (Light 0) $= if' l Enabled Disabled
  lookAt (Vertex3 0.0 0.0 z) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  rotate (xangle + y) $ Vector3 1.0 0.0 0.0
  rotate (yangle + x) $ Vector3 0.0 1.0 0.0
--  rotate (zangle + fromIntegral z') $ Vector3 0.0 0.0 1.0
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
        , "Char 'r' -> get ps >>= \\p -> ps $$! p & cameraShift .~ defaultState ^. cameraShift"
        , "Char 'x' -> onCoord cX succ"
        , "Char 'y' -> onCoord cY succ"
        , "Char 'z' -> onCoord cZ succ"
        , "Char 'X' -> onCoord cX pred"
        , "Char 'Y' -> onCoord cY pred"
        , "Char 'Z' -> onCoord cZ pred"
        , "Char 's' -> toggleFlag ps Flags"
        , "Char 'i' -> toggleFlag ps Main.Info"
        , "Char 'e' -> toggleFlag ps ExtraInfo"
        , "Char 'd' -> toggleFlag ps DragInfo"
        , "Char 'm' -> toggleFlag ps MouseInfo"
        , "Char 'h' -> toggleFlag ps Main.Help"
        , "Char 's' -> toggleFlag ps ShiftInfo"
        ]


  let h f g = if f `elem` p ^. flags then [p ^. g ^. to show] else []
      info = (if ExtraInfo `elem` p ^. flags then p ^. extraInfo else [])
             ++ h MouseInfo mouseState ++ h DragInfo dragState
             ++ h ShiftInfo cameraShift ++ h Flags flags
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
        let CameraShift sx sy sz = pState ^. cameraShift
            xDifference = newX - oldX + sx
            yDifference = newY - oldY + sy

        let p' = pState & cameraShift .~ CameraShift xDifference yDifference sz
        ps $$! p' &  dragState . _2 .~ Just p
        writeLog ps p

      else do ps $$! pState & dragState .~ (nowMS, Just p)
              writeLog ps p


keyboardMouse :: IORef ProgramState -> KeyboardMouseCallback
keyboardMouse ps key Down _ _ = case key of
  Char 'q' -> exitSuccess
  Char 'f'-> toggleFlag ps Main.Fill
  Char 'l' -> get ps >>= \p -> ps $$! p & lightState %~ not
  Char 'r' -> get ps >>= \p -> ps $$! p & cameraShift .~ defaultState ^. cameraShift
  Char 'x' -> onCoord cX succ
  Char 'y' -> onCoord cY succ
  Char 'z' -> onCoord cZ succ
  Char 'X' -> onCoord cX pred
  Char 'Y' -> onCoord cY pred
  Char 'Z' -> onCoord cZ pred
  Char 's' -> toggleFlag ps Flags
  Char 'i' -> toggleFlag ps Main.Info
  Char 'e' -> toggleFlag ps ExtraInfo
  Char 'd' -> toggleFlag ps DragInfo
  Char 'm' -> toggleFlag ps MouseInfo
  Char 'h' -> toggleFlag ps Main.Help
  Char 't' -> toggleFlag ps ShiftInfo

  MouseButton LeftButton -> setB leftButton
  MouseButton RightButton -> setB rightButton
  MouseButton MiddleButton -> setB middleButton
  MouseButton WheelDown -> get ps >>= \p -> ps $$! p & cameraShift . cZ %~ pred
  _ -> print "Mouse down"
  where
    setB f = do
      p <- get ps
      ps $$! p & mouseState . f .~ Down & dragState . _2 .~ Nothing
      clearLog ps

    onCoord f g = get ps >>= \p -> ps $$! p & cameraShift . f %~ g

keyboardMouse ps key Up _ _ = case key of
  MouseButton LeftButton -> setB leftButton >> clearLog ps
  MouseButton RightButton -> setB rightButton
  MouseButton MiddleButton -> setB middleButton
  MouseButton WheelUp -> get ps >>= \p -> ps $$! p & cameraShift . cZ %~ succ
  _ -> print "Mouse up"
  where
    setB f = get ps >>= \p -> ps $$! p & mouseState . f .~ Up
