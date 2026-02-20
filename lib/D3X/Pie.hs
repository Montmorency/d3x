module D3X.Pie
    ( -- * Arc Configuration
      ArcConfig(..)
    , defaultArcConfig
      -- * Arc Data
    , ArcDatum(..)
      -- * Arc Path Generation
    , arcBuilder
    , arcPath
      -- * Pie Layout
    , pieLayout
      -- * Corner Tangent Geometry (exported for testing)
    , CornerTangent(..)
    , cornerTangents
    ) where

import D3X.Prelude
import D3X.Path
import Control.Monad (when)

-- | Configuration for the arc generator (shared across all segments)
data ArcConfig = ArcConfig
    { acInnerRadius  :: !Double -- ^ Inner radius (0 for pie, >0 for donut)
    , acOuterRadius  :: !Double -- ^ Outer radius
    , acCornerRadius :: !Double -- ^ Corner rounding radius (0 for sharp corners)
    , acPadAngle     :: !Double -- ^ Angular padding between segments (radians)
    } deriving (Eq, Show)

defaultArcConfig :: ArcConfig
defaultArcConfig = ArcConfig
    { acInnerRadius  = 0
    , acOuterRadius  = 100
    , acCornerRadius = 0
    , acPadAngle     = 0
    }

-- | Per-segment arc angles in radians.
--   Uses d3 convention: 0 = 12 o'clock, increasing clockwise.
data ArcDatum = ArcDatum
    { adStartAngle :: !Double
    , adEndAngle   :: !Double
    } deriving (Eq, Show)

-- | Result of the corner tangent intersection computation.
--   Used internally by the arc generator for rounded corners.
data CornerTangent = CornerTangent
    { ctCx  :: !Double
    , ctCy  :: !Double
    , ctX01 :: !Double
    , ctY01 :: !Double
    , ctX11 :: !Double
    , ctY11 :: !Double
    } deriving (Eq, Show)

epsilon :: Double
epsilon = 1e-12

------------------------------------------------------------------------
-- Arc path generation (ported from d3-shape arc)
------------------------------------------------------------------------

-- | Generate SVG path data string for an arc segment.
arcPath :: ArcConfig -> ArcDatum -> PathStr
arcPath config datum = buildPath (arcBuilder config datum)

-- | Build arc path commands in the PathBuilder monad.
--   Ported from d3-shape's arc generator.
--
--   Angles use d3 convention: 0 = 12 o'clock, increasing clockwise.
--   Internally offset by -pi/2 to convert to standard math convention.
arcBuilder :: ArcConfig -> ArcDatum -> PathBuilder ()
arcBuilder ArcConfig{..} ArcDatum{..} = do
    let halfPi = pi / 2
        tau    = 2 * pi

        -- Convert from d3 angle convention to math convention
        a0 = adStartAngle - halfPi
        a1 = adEndAngle - halfPi
        da = abs (a1 - a0)
        cw = a1 > a0

        -- Ensure outer >= inner
        (r0, r1) = if acOuterRadius < acInnerRadius
                   then (acOuterRadius, acInnerRadius)
                   else (acInnerRadius, acOuterRadius)

    if not (r1 > epsilon)
        then
            -- Degenerate: just a point
            moveTo (M (0, 0))

        else if da > tau - epsilon
        then do
            -- Full circle or annulus
            moveTo (M (r1 * cos a0, r1 * sin a0))
            canvasArc 0 0 r1 a0 a1 (not cw)
            when (r0 > epsilon) $ do
                moveTo (M (r0 * cos a1, r0 * sin a1))
                canvasArc 0 0 r0 a1 a0 cw

        else do
            -- Circular or annular sector
            let ap = acPadAngle / 2
                rc = min (abs (r1 - r0) / 2) acCornerRadius
                rp = if ap > epsilon
                     then sqrt (r0 * r0 + r1 * r1)
                     else 0

                -- Compute padded angles for inner and outer rings
                (a01, a11, da1, a00, a10, da0) =
                    if rp > epsilon
                    then
                        let p0 = if r0 > epsilon
                                 then asin (min 1 (rp / r0 * sin ap))
                                 else 0
                            p1 = if r1 > epsilon
                                 then asin (min 1 (rp / r1 * sin ap))
                                 else 0

                            da0' = da - p0 * 2
                            (a00', a10', da0'')
                                | da0' > epsilon =
                                    let pp = (if cw then 1 else -1) * p0
                                    in (a0 + pp, a1 - pp, da0')
                                | otherwise =
                                    ((a0 + a1) / 2, (a0 + a1) / 2, 0)

                            da1' = da - p1 * 2
                            (a01', a11', da1'')
                                | da1' > epsilon =
                                    let pp = (if cw then 1 else -1) * p1
                                    in (a0 + pp, a1 - pp, da1')
                                | otherwise =
                                    ((a0 + a1) / 2, (a0 + a1) / 2, 0)
                        in (a01', a11', da1'', a00', a10', da0'')
                    else
                        (a0, a1, da, a0, a1, da)

                x01 = r1 * cos a01
                y01 = r1 * sin a01
                x10 = r0 * cos a10
                y10 = r0 * sin a10

                -- Compute effective corner radii, restricted by sector angle
                (rc0, rc1) =
                    if rc > epsilon
                    then
                        let x11 = r1 * cos a11
                            y11 = r1 * sin a11
                            x00 = r0 * cos a00
                            y00 = r0 * sin a00
                        in if da < pi
                           then case lineIntersect x01 y01 x00 y00 x11 y11 x10 y10 of
                               Just (ocx, ocy) ->
                                   let ax = x01 - ocx
                                       ay = y01 - ocy
                                       bx = x11 - ocx
                                       by = y11 - ocy
                                       kc = 1 / sin (acos (clamp (-1) 1 $
                                                (ax * bx + ay * by)
                                              / (sqrt (ax * ax + ay * ay)
                                               * sqrt (bx * bx + by * by))) / 2)
                                       lc = sqrt (ocx * ocx + ocy * ocy)
                                   in ( min rc ((r0 - lc) / (kc - 1))
                                      , min rc ((r1 - lc) / (kc + 1)))
                               Nothing -> (0, 0)
                           else (rc, rc)
                    else (0, 0)

            -----------------------------------------------------------
            -- Draw outer ring
            -----------------------------------------------------------
            if not (da1 > epsilon)
                then
                    -- Sector collapsed to a line
                    moveTo (M (x01, y01))

                else if rc1 > epsilon
                then do
                    -- Outer ring with rounded corners
                    let x11 = r1 * cos a11
                        y11 = r1 * sin a11
                        x00 = r0 * cos a00
                        y00 = r0 * sin a00
                        t0  = cornerTangents x00 y00 x01 y01 r1 rc1 cw
                        t1  = cornerTangents x11 y11 x10 y10 r1 rc1 cw

                    moveTo (M (ctCx t0 + ctX01 t0, ctCy t0 + ctY01 t0))

                    if rc1 < rc
                        then
                            -- Corners have merged
                            canvasArc (ctCx t0) (ctCy t0) rc1
                                (atan2 (ctY01 t0) (ctX01 t0))
                                (atan2 (ctY01 t1) (ctX01 t1))
                                (not cw)
                        else do
                            -- Two corners and the ring between them
                            canvasArc (ctCx t0) (ctCy t0) rc1
                                (atan2 (ctY01 t0) (ctX01 t0))
                                (atan2 (ctY11 t0) (ctX11 t0))
                                (not cw)
                            canvasArc 0 0 r1
                                (atan2 (ctCy t0 + ctY11 t0) (ctCx t0 + ctX11 t0))
                                (atan2 (ctCy t1 + ctY11 t1) (ctCx t1 + ctX11 t1))
                                (not cw)
                            canvasArc (ctCx t1) (ctCy t1) rc1
                                (atan2 (ctY11 t1) (ctX11 t1))
                                (atan2 (ctY01 t1) (ctX01 t1))
                                (not cw)

                else do
                    -- Outer ring as a simple circular arc
                    moveTo (M (x01, y01))
                    canvasArc 0 0 r1 a01 a11 (not cw)

            -----------------------------------------------------------
            -- Draw inner ring
            -----------------------------------------------------------
            if not (r0 > epsilon) || not (da0 > epsilon)
                then
                    -- No inner ring, or annular sector collapsed due to padding
                    lineTo (L (x10, y10))

                else if rc0 > epsilon
                then do
                    -- Inner ring with rounded corners
                    let x11 = r1 * cos a11
                        y11 = r1 * sin a11
                        x00 = r0 * cos a00
                        y00 = r0 * sin a00
                        t0  = cornerTangents x10 y10 x11 y11 r0 (negate rc0) cw
                        t1  = cornerTangents x01 y01 x00 y00 r0 (negate rc0) cw

                    lineTo (L (ctCx t0 + ctX01 t0, ctCy t0 + ctY01 t0))

                    if rc0 < rc
                        then
                            -- Corners have merged
                            canvasArc (ctCx t0) (ctCy t0) rc0
                                (atan2 (ctY01 t0) (ctX01 t0))
                                (atan2 (ctY01 t1) (ctX01 t1))
                                (not cw)
                        else do
                            -- Two corners and the ring between them
                            canvasArc (ctCx t0) (ctCy t0) rc0
                                (atan2 (ctY01 t0) (ctX01 t0))
                                (atan2 (ctY11 t0) (ctX11 t0))
                                (not cw)
                            canvasArc 0 0 r0
                                (atan2 (ctCy t0 + ctY11 t0) (ctCx t0 + ctX11 t0))
                                (atan2 (ctCy t1 + ctY11 t1) (ctCx t1 + ctX11 t1))
                                cw
                            canvasArc (ctCx t1) (ctCy t1) rc0
                                (atan2 (ctY11 t1) (ctX11 t1))
                                (atan2 (ctY01 t1) (ctX01 t1))
                                (not cw)

                else
                    -- Inner ring as a simple circular arc
                    canvasArc 0 0 r0 a10 a00 cw

    closePath Z

------------------------------------------------------------------------
-- Pie layout
------------------------------------------------------------------------

-- | Compute arc data for a pie chart from a list of values.
--   Maps values proportionally to angles in [0, 2*pi].
--
-- @
-- pieLayout [2, 3, 5]
-- -- [ ArcDatum 0.0 1.2566..    -- 2/10 of circle
-- -- , ArcDatum 1.2566.. 3.1415.. -- 3/10 of circle
-- -- , ArcDatum 3.1415.. 6.2831.. -- 5/10 of circle
-- -- ]
-- @
pieLayout :: [Double] -> [ArcDatum]
pieLayout [] = []
pieLayout values =
    let total = sum values
        tau   = 2 * pi
        cumulative = scanl (+) 0 values
        starts = init cumulative
        ends   = tail cumulative
        toAngle v
            | total > 0 = v / total * tau
            | otherwise = 0
    in zipWith (\s e -> ArcDatum (toAngle s) (toAngle e)) starts ends

------------------------------------------------------------------------
-- Corner tangent geometry (ported from d3-shape)
------------------------------------------------------------------------

-- | Compute perpendicular offset line of length rc.
--   Used for computing corner tangent intersection points.
--   Ported from d3-shape's cornerTangents function.
--
--   Reference: <http://mathworld.wolfram.com/Circle-LineIntersection.html>
cornerTangents :: Double -> Double -> Double -> Double
              -> Double -> Double -> Bool -> CornerTangent
cornerTangents x0 y0 x1 y1 r1 rc cw' =
    let x01  = x0 - x1
        y01  = y0 - y1
        lo   = (if cw' then rc else negate rc) / sqrt (x01 * x01 + y01 * y01)
        ox   = lo * y01
        oy   = negate (lo * x01)
        x11  = x0 + ox
        y11  = y0 + oy
        x10  = x1 + ox
        y10  = y1 + oy
        x00  = (x11 + x10) / 2
        y00  = (y11 + y10) / 2
        dx   = x10 - x11
        dy   = y10 - y11
        d2   = dx * dx + dy * dy
        r    = r1 - rc
        bigD = x11 * y10 - x10 * y11
        d    = (if dy < 0 then -1 else 1) * sqrt (max 0 (r * r * d2 - bigD * bigD))
        cx0' = ( bigD * dy - dx * d) / d2
        cy0' = (negate (bigD * dx) - dy * d) / d2
        cx1' = ( bigD * dy + dx * d) / d2
        cy1' = (negate (bigD * dx) + dy * d) / d2
        dx0  = cx0' - x00
        dy0  = cy0' - y00
        dx1  = cx1' - x00
        dy1  = cy1' - y00
        -- Pick the closer of the two intersection points
        (cx0, cy0) = if dx0 * dx0 + dy0 * dy0 > dx1 * dx1 + dy1 * dy1
                     then (cx1', cy1')
                     else (cx0', cy0')
    in CornerTangent
        { ctCx  = cx0
        , ctCy  = cy0
        , ctX01 = negate ox
        , ctY01 = negate oy
        , ctX11 = cx0 * (r1 / r - 1)
        , ctY11 = cy0 * (r1 / r - 1)
        }

-- | Line-line intersection. Returns Nothing if lines are parallel.
--   Ported from d3-shape's intersect function.
lineIntersect :: Double -> Double -> Double -> Double
             -> Double -> Double -> Double -> Double
             -> Maybe Point
lineIntersect x0 y0 x1 y1 x2 y2 x3 y3 =
    let x10   = x1 - x0
        y10   = y1 - y0
        x32   = x3 - x2
        y32   = y3 - y2
        denom = y32 * x10 - x32 * y10
    in if denom * denom < epsilon
       then Nothing
       else let t = (x32 * (y0 - y2) - y32 * (x0 - x2)) / denom
            in Just (x0 + t * x10, y0 + t * y10)

-- | Clamp a value to [lo, hi]
clamp :: Double -> Double -> Double -> Double
clamp lo hi x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise = x
