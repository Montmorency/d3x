{-# LANGUAGE ImplicitPrelude #-}
-- | Adaptive geodesic line subdivision.
--
--   Mirrors @d3-geo/src/projection/resample.js@ — recursively inserts
--   midpoints between two emitted points whenever the projected
--   midpoint deviates from the straight chord by more than @sqrt(delta2)@,
--   so that geodesic curvature on Mercator / Conic / Orthographic
--   renders as smooth curves rather than jagged polylines.
module D3X.Geo.Resample
    ( resampleNone
    , resample
    ) where

import D3X.Geo.Cartesian
    ( Cart, sphericalToCartesian, cartesianAdd, cartesianNormalize )
import D3X.Geo.Math (epsilon)
import D3X.Geo.Raw (RawProjection(..))
import D3X.Geo.Stream (Stream(..), mapPoint)

-- | Pass each point through the raw projection unchanged. No subdivision.
--   This is what d3-geo uses when @precision == 0@ (e.g. 'geoIdentity').
resampleNone :: RawProjection -> Stream r -> Stream r
resampleNone proj = mapPoint (rawProject proj)

------------------------------------------------------------------------
-- Adaptive resample
------------------------------------------------------------------------

data ResamplePoint = ResamplePoint
    { rpLam  :: !Double
    , rpPhi  :: !Double
    , rpX    :: !Double
    , rpY    :: !Double
    , rpCart :: !Cart
    }

-- | The state of the resample wrapper.
data Mode
    = TopMode                                                -- ^ outside any line / polygon
    | LineMode  !(Maybe ResamplePoint)                       -- ^ inside a free LineString
    | InPolygon                                              -- ^ between polygonStart and the first ring's lineStart, or between rings
    | RingMode  !(Maybe ResamplePoint) !(Maybe ResamplePoint)
        -- ^ @(first ring point, previous ring point)@

-- | Adaptive resample. Delegates to 'resampleNone' when @delta2 == 0@.
resample :: RawProjection -> Double -> Stream r -> Stream r
resample proj delta2
    | delta2 <= 0 = resampleNone proj
    | otherwise   = wrap TopMode
  where
    project       = rawProject proj
    maxDepth      = 16 :: Int
    cosMinDistance = cos (30 * pi / 180)

    mkPoint :: Double -> Double -> ResamplePoint
    mkPoint lam phi =
        let (x, y) = project lam phi
            c      = sphericalToCartesian lam phi
        in ResamplePoint lam phi x y c

    wrap mode inner = Stream
        { streamPoint        = \lam phi -> handlePoint mode inner lam phi
        , streamLineStart    = handleLineStart mode inner
        , streamLineEnd      = handleLineEnd   mode inner
        , streamPolygonStart = wrap InPolygon (streamPolygonStart inner)
        , streamPolygonEnd   = wrap TopMode   (streamPolygonEnd   inner)
        , streamSphere       = wrap mode      (streamSphere       inner)
        , streamResult       = streamResult inner
        }

    handleLineStart mode inner = case mode of
        InPolygon -> wrap (RingMode Nothing Nothing) (streamLineStart inner)
        _         -> wrap (LineMode Nothing)         (streamLineStart inner)

    handleLineEnd mode inner = case mode of
        RingMode (Just first) (Just lastP) ->
            let inner1 = resampleLineTo lastP first maxDepth inner
            in wrap InPolygon (streamLineEnd inner1)
        InPolygon -> wrap InPolygon (streamLineEnd inner)
        _         -> wrap TopMode   (streamLineEnd inner)

    handlePoint mode inner lam phi = case mode of
        TopMode ->
            let (px, py) = project lam phi
            in wrap TopMode (streamPoint inner px py)

        LineMode Nothing ->
            let p = mkPoint lam phi
            in wrap (LineMode (Just p)) (streamPoint inner (rpX p) (rpY p))

        LineMode (Just p0) ->
            let p1     = mkPoint lam phi
                inner1 = resampleLineTo p0 p1 maxDepth inner
                inner2 = streamPoint inner1 (rpX p1) (rpY p1)
            in wrap (LineMode (Just p1)) inner2

        InPolygon ->
            -- bare point inside polygon (rare); just emit projected
            let (px, py) = project lam phi
            in wrap InPolygon (streamPoint inner px py)

        RingMode Nothing _ ->
            let p = mkPoint lam phi
            in wrap (RingMode (Just p) (Just p)) (streamPoint inner (rpX p) (rpY p))

        RingMode (Just first) (Just p0) ->
            let p1     = mkPoint lam phi
                inner1 = resampleLineTo p0 p1 maxDepth inner
                inner2 = streamPoint inner1 (rpX p1) (rpY p1)
            in wrap (RingMode (Just first) (Just p1)) inner2

        RingMode (Just first) Nothing ->
            -- Should not happen (ring is initialised with both Just), but be safe.
            wrap (RingMode (Just first) Nothing) inner

    -- Recursively subdivide the geodesic between @p0@ and @p1@,
    -- emitting interior points to @inner@ when the projected
    -- midpoint deviates from the straight chord.
    resampleLineTo p0 p1 depth inner
        | depth <= 0       = inner
        | d2 <= 4 * delta2 = inner
        | otherwise =
            let midC@(am, bm, cm) = cartesianNormalize (cartesianAdd (rpCart p0) (rpCart p1))
                phi2 = asin cm
                lam2 = if abs (abs cm - 1) < epsilon
                          || abs (rpLam p0 - rpLam p1) < epsilon
                          then (rpLam p0 + rpLam p1) / 2
                          else atan2 bm am
                (x2, y2) = project lam2 phi2
                pMid = ResamplePoint lam2 phi2 x2 y2 midC
                dx2  = x2 - rpX p0
                dy2  = y2 - rpY p0
                dz   = dy * dx2 - dx * dy2
                shouldSubdivide =
                       (dz * dz) / d2 > delta2
                    || abs ((dx * dx2 + dy * dy2) / d2 - 0.5) > 0.3
                    || dotProd < cosMinDistance
            in if shouldSubdivide
                   then
                       let inner1 = resampleLineTo p0   pMid (depth - 1) inner
                           inner2 = streamPoint inner1 x2 y2
                           inner3 = resampleLineTo pMid p1   (depth - 1) inner2
                       in inner3
                   else inner
      where
        dx = rpX p1 - rpX p0
        dy = rpY p1 - rpY p0
        d2 = dx * dx + dy * dy
        (a0, b0, c0) = rpCart p0
        (a1, b1, c1) = rpCart p1
        dotProd = a0 * a1 + b0 * b1 + c0 * c1
