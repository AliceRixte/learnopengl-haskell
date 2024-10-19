{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Fast(ish) rendering of circles.
module Circle
        ( module Circle
        , renderCircle
        , renderArc)
where
-- import  Graphics.Gloss.Internals.Rendering.Common
import  GHC.Exts
import  qualified Graphics.Rendering.OpenGL.GL          as GL
import Data.Data

import Unsafe.Coerce


-- | An abstract color value.
--      We keep the type abstract so we can be sure that the components
--      are in the required range. To make a custom color use 'makeColor'.
data Color
        -- | Holds the color components. All components lie in the range [0..1.
        = RGBA  !Float !Float !Float !Float
        deriving (Show, Eq, Data, Typeable)


instance Num Color where
 (+) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 + r2) (g1 + g2) (b1 + b2) 1
 {-# INLINE (+) #-}

 (-) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 - r2) (g1 - g2) (b1 - b2) 1
 {-# INLINE (-) #-}

 (*) (RGBA r1 g1 b1 _) (RGBA r2 g2 b2 _)
        = RGBA (r1 * r2) (g1 * g2) (b1 * b2) 1
 {-# INLINE (*) #-}

 abs (RGBA r1 g1 b1 _)
        = RGBA (abs r1) (abs g1) (abs b1) 1
 {-# INLINE abs #-}

 signum (RGBA r1 g1 b1 _)
        = RGBA (signum r1) (signum g1) (signum b1) 1
 {-# INLINE signum #-}

 fromInteger i
  = let f = fromInteger i
    in  RGBA f f f 1
 {-# INLINE fromInteger #-}


-- | Make a custom color. All components are clamped to the range  [0..1].
makeColor
        :: Float        -- ^ Red component.
        -> Float        -- ^ Green component.
        -> Float        -- ^ Blue component.
        -> Float        -- ^ Alpha component.
        -> Color

makeColor r g b a
        = clampColor
        $ RGBA r g b a
{-# INLINE makeColor #-}


-- | Make a custom color. All components are clamped to the range [0..255].
makeColorI :: Int -> Int -> Int -> Int -> Color
makeColorI r g b a
        = clampColor
        $ RGBA  (fromIntegral r / 255)
                (fromIntegral g / 255)
                (fromIntegral b / 255)
                (fromIntegral a / 255)
{-# INLINE makeColorI #-}


-- | Make a custom color.
--
--   Using this function over `makeColor` avoids clamping the components,
--   which saves time. However, if the components are out of range then
--   this will result in integer overflow at rendering time, and the actual
--   picture you get will be implementation dependent.
--
--   You'll only need to use this function when using the @gloss-raster@
--   package that builds a new color for every pixel. If you're just working
--   with the Picture data type then it there is no need for raw colors.
--
makeRawColor :: Float -> Float -> Float -> Float -> Color
makeRawColor r g b a
        = RGBA r g b a
{-# INLINE makeRawColor #-}


-- | Make a custom color, taking pre-clamped components.
makeRawColorI :: Int -> Int -> Int -> Int -> Color
makeRawColorI r g b a
        = RGBA  (fromIntegral r / 255)
                (fromIntegral g / 255)
                (fromIntegral b / 255)
                (fromIntegral a / 255)
{-# INLINE makeRawColorI #-}


-- | Take the RGBA components of a color.
rgbaOfColor :: Color -> (Float, Float, Float, Float)
rgbaOfColor (RGBA r g b a)      = (r, g, b, a)
{-# INLINE rgbaOfColor #-}


-- | Clamp components of a raw color into the required range.
clampColor :: Color -> Color
clampColor cc
 = let  (r, g, b, a)    = rgbaOfColor cc
        clamp x         = (min (max x 0.0) 1.0)
   in   RGBA (clamp r) (clamp g) (clamp b) (clamp a)

------------------------------------------------------------------------------
glColor4OfColor :: Color -> GL.Color4 a
glColor4OfColor color
 = case color of
        RGBA r g b a
         -> let rF      = unsafeCoerce r
                gF      = unsafeCoerce g
                bF      = unsafeCoerce b
                aF      = unsafeCoerce a
            in  GL.Color4 rF gF bF aF
{-# INLINE glColor4OfColor #-}


------------------------------------------------------------------------------

-- | The OpenGL library doesn't seem to provide a nice way convert
--      a Float to a GLfloat, even though they're the same thing
--      under the covers.
--
--  Using realToFrac is too slow, as it doesn't get fused in at
--      least GHC 6.12.1
--
gf :: Float -> GL.GLfloat
gf x = unsafeCoerce x
{-# INLINE gf #-}


-- | Used for similar reasons to above
gsizei :: Int -> GL.GLsizei
gsizei x = unsafeCoerce x
{-# INLINE gsizei #-}


-- | Set up the OpenGL rendering context for orthographic projection and run an
--   action to draw the model.
withModelview
        :: (Int, Int)  -- ^ Width and height of window.
        -> IO ()       -- ^ Action to perform.
        -> IO ()

withModelview (sizeX, sizeY) action
 = do
        GL.matrixMode   GL.$= GL.Projection
        GL.preservingMatrix
         $ do
                -- setup the co-ordinate system
                GL.loadIdentity
                let (sx, sy)    = (fromIntegral sizeX / 2, fromIntegral sizeY / 2)
                GL.ortho (-sx) sx (-sy) sy 0 (-100)

                -- draw the world
                GL.matrixMode   GL.$= GL.Modelview 0
                action

                GL.matrixMode   GL.$= GL.Projection

        GL.matrixMode   GL.$= GL.Modelview 0


-- | Clear the OpenGL buffer with the given background color and run
--   an action to draw the model.
withClearBuffer
        :: Color        -- ^ Background color
        -> IO ()        -- ^ Action to perform
        -> IO ()

withClearBuffer clearColor action
 = do
        -- initialization (done every time in this case)
        -- we don't need the depth buffer for 2d.
        GL.depthFunc    GL.$= Just GL.Always

        -- always clear the buffer to white
        GL.clearColor   GL.$= glColor4OfColor clearColor

        -- on every loop
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

        action



-------------------------------------------------------------------------------
-- | Decide how many line segments to use to render the circle.
--   The number of segments we should use to get a nice picture depends on
--   the size of the circle on the screen, not its intrinsic radius.
--   If the viewport has been zoomed-in then we need to use more segments.
circleSteps :: Float -> Int
circleSteps sDiam
        | sDiam < 8     = 8
        | sDiam < 16    = 16
        | sDiam < 32    = 32
        | otherwise     = 64
{-# INLINE circleSteps #-}


-- Circle ---------------------------------------------------------------------
-- | Render a circle with the given thickness
renderCircle :: Float -> Float -> Float -> Float -> Float -> IO ()
renderCircle posX posY scaleFactor radius_ thickness_
 = go (abs radius_) (abs thickness_)
 where go radius thickness

        -- If the circle is smaller than a pixel, render it as a point.
        | thickness     == 0
        , radScreen     <- scaleFactor * (radius + thickness / 2)
        , radScreen     <= 1
        = GL.renderPrimitive GL.Points
            $ GL.vertex $ GL.Vertex2 (gf posX) (gf posY)

        -- Render zero thickness circles with lines.
        | thickness == 0
        , radScreen     <- scaleFactor * radius
        , steps         <- circleSteps radScreen
        = renderCircleLine  posX posY steps radius

        -- Some thick circle.
        | radScreen     <- scaleFactor * (radius + thickness / 2)
        , steps         <- circleSteps radScreen
        = renderCircleStrip posX posY steps radius thickness


-- | Render a circle as a line.
renderCircleLine :: Float -> Float -> Int -> Float -> IO ()
renderCircleLine (F# posX) (F# posY) steps (F# rad)
 = let  n               = fromIntegral steps
        !(F# tStep)     = (2 * pi) / n
        !(F# tStop)     = (2 * pi)

   in   GL.renderPrimitive GL.LineLoop
         $ renderCircleLine_step posX posY tStep tStop rad 0.0#
{-# INLINE renderCircleLine #-}


-- | Render a circle with a given thickness as a triangle strip
renderCircleStrip :: Float -> Float -> Int -> Float -> Float -> IO ()
renderCircleStrip (F# posX) (F# posY) steps r width
 = let  n               = fromIntegral steps
        !(F# tStep)     = (2 * pi) / n
        !(F# tStop)     = (2 * pi) + (F# tStep) / 2
        !(F# r1)        = r - width / 2
        !(F# r2)        = r + width / 2

   in   GL.renderPrimitive GL.TriangleStrip
         $ renderCircleStrip_step posX posY tStep tStop r1 0.0# r2
                (tStep `divideFloat#` 2.0#)
{-# INLINE renderCircleStrip #-}


-- Arc ------------------------------------------------------------------------
-- | Render an arc with the given thickness.
renderArc
 :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()
renderArc posX posY scaleFactor radius_ a1 a2 thickness_
 = go (abs radius_) (abs thickness_)
 where
       go radius thickness
        -- Render zero thickness arcs with lines.
        | thickness == 0
        , radScreen     <- scaleFactor * radius
        , steps         <- circleSteps radScreen
        = renderArcLine posX posY steps radius a1 a2

        -- Some thick arc.
        | radScreen     <- scaleFactor * (radius + thickness / 2)
        , steps         <- circleSteps radScreen
        = renderArcStrip posX posY steps radius a1 a2 thickness


-- | Render an arc as a line.
renderArcLine
 :: Float -> Float -> Int -> Float -> Float -> Float -> IO ()
renderArcLine (F# posX) (F# posY) steps (F# rad) a1 a2
 = let  n               = fromIntegral steps
        !(F# tStep)     = (2 * pi) / n
        !(F# tStart)    = degToRad a1
        !(F# tStop)     = degToRad a2 + if a1 >= a2 then 2 * pi else 0

        -- force the line to end at the desired angle
        endVertex       = addPointOnCircle posX posY rad tStop

   in   GL.renderPrimitive GL.LineStrip
         $ do   renderCircleLine_step posX posY tStep tStop rad tStart
                endVertex
{-# INLINE renderArcLine #-}


-- | Render an arc with a given thickness as a triangle strip
renderArcStrip
 :: Float -> Float -> Int -> Float -> Float -> Float -> Float -> IO ()
renderArcStrip (F# posX) (F# posY) steps r a1 a2 width
 = let  n               = fromIntegral steps
        tStep           = (2 * pi) / n

        t1              = normalizeAngle $ degToRad a1

        a2'             = normalizeAngle $ degToRad a2
        t2              = if a2' == 0 then 2*pi else a2'

        (tStart, tStop) = if t1 <= t2 then (t1, t2) else (t2, t1)
        tDiff           = tStop - tStart
        tMid            = tStart + tDiff / 2

        !(F# tStep')    = tStep
        !(F# tStep2')   = tStep / 2
        !(F# tStart')   = tStart
        !(F# tStop')    = tStop
        !(F# tCut')     = tStop - tStep
        !(F# tMid')     = tMid
        !(F# r1')       = r - width / 2
        !(F# r2')       = r + width / 2

   in   GL.renderPrimitive GL.TriangleStrip
         $ do   -- start vector
                addPointOnCircle posX posY r1' tStart'
                addPointOnCircle posX posY r2' tStart'

                -- If we don't have a complete step then just drop a point
                -- between the two ending lines.
                if tDiff < tStep
                  then do
                        addPointOnCircle posX posY r1' tMid'

                        -- end vectors
                        addPointOnCircle posX posY r2' tStop'
                        addPointOnCircle posX posY r1' tStop'

                  else do
                        renderCircleStrip_step posX posY
                                tStep' tCut' r1' tStart' r2'
                                (tStart' `plusFloat#` tStep2')

                        -- end vectors
                        addPointOnCircle posX posY r1' tStop'
                        addPointOnCircle posX posY r2' tStop'
{-# INLINE renderArcStrip #-}


-- Step functions -------------------------------------------------------------
renderCircleLine_step
        :: Float# -> Float#
        -> Float# -> Float#
        -> Float# -> Float#
        -> IO ()

renderCircleLine_step posX posY tStep tStop rad tt
        | 1# <- tt `geFloat#` tStop
        = return ()

        | otherwise
        = do    addPointOnCircle posX posY rad tt
                renderCircleLine_step posX posY tStep tStop rad
                        (tt `plusFloat#` tStep)
{-# INLINE renderCircleLine_step #-}


renderCircleStrip_step
        :: Float# -> Float#
        -> Float# -> Float#
        -> Float# -> Float#
        -> Float# -> Float# -> IO ()

renderCircleStrip_step posX posY tStep tStop r1 t1 r2 t2
        | 1# <- t1 `geFloat#` tStop
        = return ()

        | otherwise
        = do    addPointOnCircle posX posY r1 t1
                addPointOnCircle posX posY r2 t2
                renderCircleStrip_step posX posY tStep tStop r1
                        (t1 `plusFloat#` tStep) r2 (t2 `plusFloat#` tStep)
{-# INLINE renderCircleStrip_step #-}


addPoint :: Float# -> Float# -> IO ()
addPoint x y =
  GL.vertex $ GL.Vertex2 (gf (F# x)) (gf (F# y))
{-# INLINE addPoint #-}


addPointOnCircle :: Float# -> Float# -> Float# -> Float# -> IO ()
addPointOnCircle posX posY rad tt =
  addPoint
    (posX `plusFloat#` (rad `timesFloat#` (cosFloat# tt)))
    (posY `plusFloat#` (rad `timesFloat#` (sinFloat# tt)))
{-# INLINE addPointOnCircle #-}


-- | Convert degrees to radians
degToRad :: Float -> Float
degToRad d      = d * pi / 180
{-# INLINE degToRad #-}


-- | Normalise an angle to be between 0 and 2*pi radians
normalizeAngle :: Float -> Float
normalizeAngle f = f - 2 * pi * floor' (f / (2 * pi))
 where  floor' :: Float -> Float
        floor' x = fromIntegral (floor x :: Int)
{-# INLINE normalizeAngle #-}