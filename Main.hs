{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.List
import Data.Time.Clock
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


data MouseState = MouseState { _leftButton :: KeyState
                             , _rightButton :: KeyState
                             , _middleButton :: KeyState
                             } deriving (Eq, Show)

data CameraShift = CameraShift { _cX :: GLdouble
                               , _cY :: GLdouble
                               , _cZ :: GLdouble
                               }
                      deriving (Eq, Show)

data Flag = Info | ExtraInfo | CameraInfo | Flags | ShiftInfo | Middle | Shapes
          | DragInfo | MouseInfo | Fill | Help | Lights | FramesPerSecond
          deriving (Show, Eq)

data ProgramState = ProgramState { _cameraShift :: CameraShift
                                 , _mouseState :: MouseState
                                 , _angleState :: GLfloat
                                 , _dragState :: (MouseState, Maybe Position)
                                 , _extraInfo :: [String]
                                 , _flags :: [Flag]
                                 , _timeState :: (DiffTime, Int, Int)
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
  { _cameraShift = CameraShift (-350) 350 40
  , _mouseState = MouseState Up Up Up
  , _angleState = 0.0
  , _dragState = (MouseState Up Up Up, Nothing)
  , _extraInfo = []
  , _flags = [ Main.Info, Flags, Lights, FramesPerSecond
             , Main.Fill, DragInfo, MouseInfo, ShiftInfo
             , Shapes
             ]
  , _timeState = (secondsToDiffTime 0, 0, 0)
  }

-- Remind $=! with to with 0 left fixity to drop some parens
($$!) :: HasSetter s => s a -> a -> IO ()
($$!) = ($=!)
infixl 0 $$!


ctvf :: CameraShift -> Vertex3 GLdouble
ctvf (CameraShift x y z) = Vertex3 x y z

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

-- dl :: Bool -> IO DisplayList
-- dl f = do
dl :: IO DisplayList
dl = do
  let scale = [0 .. 3]
      coords :: [Vector3 GLdouble]
      coords = nub [ Vector3 a b c | a <- scale, b <- scale, c <- scale ]
  color $ Color3 1 0 (0 :: GLdouble)
  defineNewList CompileAndExecute $
    flip mapM_ coords $ do
      \v@(Vector3 x y z) -> preservingMatrix $ do
        translate v
        renderObject Solid (Cube 0.2) -- 10 10)
        -- drawCube True

-- display :: IORef ProgramState -> IO ()
-- display ps = do
--   programState <- get ps
--   let f = Main.Fill `elem` programState ^. flags
--   clear [ColorBuffer, DepthBuffer]
--   when (Shapes `elem` programState ^. flags) (preservingMatrix $ drawCube f)

--   updateTime ps

--   preservingMatrix (renderInfo programState)

--   swapBuffers

updateTime :: IORef ProgramState -> IO ()
updateTime ps = do
  programState <- get ps
  t <- utctDayTime <$> getCurrentTime
  let oldTime = programState ^. timeState . _1
  let p' = if t - oldTime >= 1
           then let allFrames = programState ^. timeState . _2
                in programState & timeState .~ (t, 0, allFrames)
           else programState

  ps $$! p' & timeState . _2 %~ succ -- & angleState %~ (+ 1.0)


display :: IORef ProgramState -> DisplayCallback
display ps = do
  programState <- get ps
  let l = Lights `elem` programState ^. flags
      a = programState ^. angleState
      f = Main.Fill `elem` programState ^. flags
      c = programState ^. cameraShift
      CameraShift x' y' z' = programState ^. cameraShift
      (x, y, z) = (realToFrac $ x' / 10, realToFrac $ y' / 10, realToFrac z')
  clear [ColorBuffer, DepthBuffer]

  preservingMatrix drawAxis

  when (Shapes `elem` programState ^. flags) (callList <$> dl  >> return ())

  -- loadIdentity
  -- pos = Vertex4 0.2 0.2 0.9 0.0
  preservingMatrix $ do
    Vertex4 x y z w <- get $ position (Light 0)
    let height = 0.4
    translate $ Vector3 x y (z - height)
    renderObject Solid (Cone 0.1 (realToFrac height) 10 10)


  loadIdentity
  preservingMatrix (renderInfo programState)
  lighting $= if' l Enabled Disabled
  let m = if Middle `elem` programState ^. flags
          then Vertex3 (0.0) 30.0 0.0
          else Vertex3 0.0 0.0 0.0
  lookAt (Vertex3 0.0 0.0 z) m (Vector3 0.0 1.0 0.0)
  rotate (xangle + y) $ Vector3 1.0 0.0 0.0
  rotate (yangle + x) $ Vector3 0.0 1.0 0.0

--  preservingMatrix (drawCube f)
--  when (Shapes `elem` programState ^. flags) (preservingMatrix $ drawCube f)

  ps $$! programState & angleState %~ (+ 1.0)

  updateTime ps

  swapBuffers
  where
    drawToLeft f x = preservingMatrix $ do
      translate $ Vector3 x 0 (0 :: GLdouble)
      preservingMatrix (drawCube f)

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
    preservingMatrix $ do
      -- X
      color $ Color3 1.0 0.0 (0.0 :: GLfloat)

      renderPrimitive Lines $ mapM_ vertex3f $
        [ (0.0, 0.0, 0.0)
        , apply3 (* len) xv
        ] ++ concat (map (\x -> [(x, 0, -1) , (x, 0, 1)] ) [step, step + step .. len])

      -- Y
      color $ Color3 0.0 1.0 (0.0 :: GLfloat)

      renderPrimitive Lines $ mapM_ vertex3f $
        [ (0.0, 0.0, 0.0)
        , apply3 (* len) yv
        ] ++ concat (map (\x -> [(-1, x, 0) , (1, x, 0)] ) [step, step + step .. len])
          ++ concat (map (\x -> [(0, x, -1) , (0, x, 1)] ) [step, step + step .. len])

      -- Z
      color $ Color3 0.0 0.0 (1.0 :: GLfloat)

      renderPrimitive Lines $ mapM_ vertex3f $
        [ (0.0, 0.0, 0.0)
        , apply3 (* len) zv
        ] ++ concat (map (\x -> [(-1, 0, x) , (1, 0, x)] ) [step, step + step .. len])

    color c

renderInfo :: ProgramState -> IO ()
renderInfo p = do

  let helpText =
        [ "Char 'q' -> exitSuccess"
        , "Char 'f'-> toggleFlag ps Main.Fill"
        , "Char 'l' -> toggleFlag ps Light"
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
        , "Char 'p' -> toggleFlag ps FramesPerSecond"
        ]


  let h f g = if f `elem` p ^. flags then [p ^. g ^. to show] else []
      info = (if ExtraInfo `elem` p ^. flags then p ^. extraInfo else [])
             ++ h MouseInfo mouseState ++ h DragInfo dragState
             ++ h ShiftInfo cameraShift ++ h Flags flags
             ++ map (++ " FPS") (h FramesPerSecond (timeState . _3))
      fps = map (++ " FPS") (h FramesPerSecond (timeState . _3))
      info' = if FramesPerSecond `elem` p ^. flags then fps else info

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
  color $ Color3 1.0 0 (0 :: GLdouble)
  drawFace filled w

  mapM_ (const $ r q 1 0 0 >> drawFace filled w) [1 .. 3 :: Integer]

  r q 1 0 0
  r q 0 1 0
  drawFace filled w

  r f 0 1 0
  drawFace filled w

  where
    w = 0.1

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
            xDifference = fromIntegral (newX - oldX) + sx
            yDifference = fromIntegral (newY - oldY) + sy

        let p' = pState & cameraShift .~ CameraShift xDifference yDifference sz
        ps $$! p' &  dragState . _2 .~ Just p
        writeLog ps p

      else do ps $$! pState & dragState .~ (nowMS, Just p)
              writeLog ps p


keyboardMouse :: IORef ProgramState -> KeyboardMouseCallback
keyboardMouse ps key Down _ _ = case key of
  Char 'q' -> exitSuccess
  Char 'f'-> toggleFlag ps Main.Fill
  Char 'r' -> get ps >>= \p -> ps $$! p & cameraShift .~ defaultState ^. cameraShift
  Char 'x' -> onCoord cX succ
  Char 'y' -> onCoord cY succ
  Char 'z' -> onCoord cZ succ
  Char 'X' -> onCoord cX pred
  Char 'Y' -> onCoord cY pred
  Char 'Z' -> onCoord cZ pred
  Char 'l' -> toggleFlag ps Lights
  Char 's' -> toggleFlag ps Flags
  Char 'i' -> toggleFlag ps Main.Info
  Char 'e' -> toggleFlag ps ExtraInfo
  Char 'd' -> toggleFlag ps DragInfo
  Char 'm' -> toggleFlag ps MouseInfo
  Char 'h' -> toggleFlag ps Main.Help
  Char 't' -> toggleFlag ps ShiftInfo
  Char 'p' -> toggleFlag ps FramesPerSecond
  Char 'M' -> toggleFlag ps Middle
  Char 'S' -> toggleFlag ps Shapes

  MouseButton LeftButton -> setB leftButton
  MouseButton RightButton -> setB rightButton
  MouseButton MiddleButton -> setB middleButton
  MouseButton WheelDown -> get ps >>= \p -> ps $$! p & cameraShift . cZ %~ succ
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
  MouseButton WheelUp -> get ps >>= \p -> ps $$! p & cameraShift . cZ %~ pred
  _ -> print "Mouse up"
  where
    setB f = get ps >>= \p -> ps $$! p & mouseState . f .~ Up
