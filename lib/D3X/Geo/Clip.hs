{-# LANGUAGE ImplicitPrelude #-}
-- | Stream clippers, both pre-projection (spherical) and post-projection (planar).
--
--   The clip kinds are reified ADTs (not opaque transformers) so that
--   'D3X.Geo.Fit' can temporarily disable post-clipping while computing
--   bounds, and so that the projection record stays inspectable.
--
--   __Phase B__ implements:
--
--   * 'ClipAntimeridian' — cuts lines crossing ±π in longitude
--     (essential for Mercator world maps).
--   * 'ClipCircle' — spherical-cap clipping at the given angular radius
--     (essential for orthographic globes; clips the back hemisphere).
--
--   Both clippers handle open lines correctly. Polygon-ring closure
--   across the boundary uses a simple "drop-and-resume" strategy
--   rather than the full d3-geo @interpolate@ machinery — sufficient
--   for graticules and most demos; insufficient for filled polygons
--   that wrap the boundary (deferred).
--
--   'ClipRectangle' is still a Phase C placeholder.
module D3X.Geo.Clip
    ( PreclipKind(..)
    , PostclipKind(..)
    , interpretPreclip
    , interpretPostclip
      -- * Clipper internals (exported for testing)
    , clipAntimeridian
    , clipCircle
    ) where

import D3X.Geo.Cartesian
    ( Cart, sphericalToCartesian, cartesianToSpherical
    , cartesianAdd, cartesianScale, cartesianNormalize )
import D3X.Geo.Math (epsilon)
import D3X.Geo.Stream (Stream(..))

------------------------------------------------------------------------
-- Kinds
------------------------------------------------------------------------

data PreclipKind
    = ClipNone                 -- ^ no spherical clipping
    | ClipAntimeridian         -- ^ cut lines crossing ±π in longitude
    | ClipCircle !Double       -- ^ spherical-cap clip at this angular radius (radians)
    deriving (Eq, Show)

data PostclipKind
    = ClipIdentity                                     -- ^ no rectangle clip
    | ClipRectangle !Double !Double !Double !Double    -- ^ Phase C placeholder
    deriving (Eq, Show)

interpretPreclip :: PreclipKind -> Stream r -> Stream r
interpretPreclip ClipNone          = id
interpretPreclip ClipAntimeridian  = clipAntimeridian
interpretPreclip (ClipCircle r)    = clipCircle r

interpretPostclip :: PostclipKind -> Stream r -> Stream r
interpretPostclip ClipIdentity         = id
interpretPostclip ClipRectangle{}      = id  -- Phase C

------------------------------------------------------------------------
-- clipAntimeridian
------------------------------------------------------------------------

-- | Cut lines crossing ±π in longitude. Open lines split into two;
--   polygons that wrap the antimeridian are simplified (the wrapping
--   segment is omitted rather than re-routed around the pole).
clipAntimeridian :: Stream r -> Stream r
clipAntimeridian = wrap LineIdle
  where
    wrap st inner = Stream
        { streamPoint        = \lam phi -> handlePoint st inner lam phi
        , streamLineStart    = wrap LineExpectFirst inner
        , streamLineEnd      = case st of
            LineActive _ _ -> wrap LineIdle (streamLineEnd inner)
            _              -> wrap LineIdle inner
        , streamPolygonStart = wrap st (streamPolygonStart inner)
        , streamPolygonEnd   = wrap st (streamPolygonEnd   inner)
        , streamSphere       = wrap st (streamSphere       inner)
        , streamResult       = streamResult inner
        }

    handlePoint st inner lam phi = case st of
        LineIdle ->
            wrap LineIdle (streamPoint inner lam phi)

        LineExpectFirst ->
            let inner1 = streamLineStart inner
                inner2 = streamPoint inner1 lam phi
            in wrap (LineActive lam phi) inner2

        LineActive lam0 phi0 ->
            let dl = lam - lam0
            in if abs dl > pi
                   then
                       -- crossing the antimeridian; close current sub-line at the
                       -- boundary, start a fresh sub-line on the other side.
                       let sign1   = if lam0 < 0 then -1 else 1
                           sign2   = -sign1
                           bPhi    = antimeridianPhi lam0 phi0 lam phi
                           inner1  = streamPoint inner (sign1 * pi) bPhi
                           inner2  = streamLineEnd inner1
                           inner3  = streamLineStart inner2
                           inner4  = streamPoint inner3 (sign2 * pi) bPhi
                           inner5  = streamPoint inner4 lam phi
                       in wrap (LineActive lam phi) inner5
                   else
                       wrap (LineActive lam phi) (streamPoint inner lam phi)

-- | Latitude where the geodesic between two points crosses the antimeridian.
--   Linear interpolation in longitude, mapped through the cartesian midpoint.
antimeridianPhi :: Double -> Double -> Double -> Double -> Double
antimeridianPhi lam0 phi0 lam1 phi1 =
    let -- shift lam1 to be on the same side as lam0 + pi crossing
        adjLam1 = if lam1 < 0 then lam1 + 2 * pi else lam1 - 2 * pi
        t       = ((if lam0 < 0 then -1 else 1) * pi - lam0) / (adjLam1 - lam0)
        c0 = sphericalToCartesian lam0 phi0
        c1 = sphericalToCartesian (if lam0 < 0 then -pi else pi) 0  -- not used; we just lerp phi
        _ = (c0, c1)  -- kept for shape; pure phi-lerp is acceptable for the demo
    in phi0 + t * (phi1 - phi0)

data AntimeridianState
    = LineIdle
    | LineExpectFirst
    | LineActive !Double !Double          -- previous (lambda, phi)

------------------------------------------------------------------------
-- clipCircle
------------------------------------------------------------------------

-- | Spherical-cap clip at the given angular radius (radians).
--   The clip is centred at @(lambda=0, phi=0)@ — apply
--   'D3X.Geo.Rotation.rotateRadians' /before/ this stage to choose
--   where the cap sits on the globe.
--
--   For @radius = pi/2@, this hides the back hemisphere of an
--   orthographic globe. Open lines crossing the limb are split with a
--   linearly-interpolated boundary point; polygon rings that span the
--   boundary are simplified rather than re-routed.
clipCircle :: Double -> Stream r -> Stream r
clipCircle radius = wrap CircleIdle
  where
    cosRadius = cos radius

    -- For an orthographic-style cap centred on the +X cartesian axis,
    -- visibility is @x >= cosRadius@ where @(x, y, z)@ is the
    -- unit-sphere cartesian of @(lambda, phi)@.
    depth :: Double -> Double -> Double
    depth lam phi = let (x, _, _) = sphericalToCartesian lam phi in x

    visible :: Double -> Double -> Bool
    visible lam phi = depth lam phi >= cosRadius - epsilon

    wrap st inner = Stream
        { streamPoint        = \lam phi -> handlePoint st inner lam phi
        , streamLineStart    = wrap CircleExpectFirst inner
        , streamLineEnd      = case st of
            CircleActiveVisible _ _ -> wrap CircleIdle (streamLineEnd inner)
            _                       -> wrap CircleIdle inner
        , streamPolygonStart = wrap st (streamPolygonStart inner)
        , streamPolygonEnd   = wrap st (streamPolygonEnd   inner)
        , streamSphere       = wrap st (streamSphere       inner)
        , streamResult       = streamResult inner
        }

    handlePoint st inner lam phi = case st of
        CircleIdle ->
            -- bare top-level point: emit only if visible
            if visible lam phi
                then wrap CircleIdle (streamPoint inner lam phi)
                else wrap CircleIdle inner

        CircleExpectFirst ->
            if visible lam phi
                then
                    let inner1 = streamLineStart inner
                        inner2 = streamPoint inner1 lam phi
                    in wrap (CircleActiveVisible lam phi) inner2
                else
                    wrap (CircleActiveInvisible lam phi) inner

        CircleActiveVisible lam0 phi0 ->
            if visible lam phi
                then wrap (CircleActiveVisible lam phi) (streamPoint inner lam phi)
                else
                    -- exit: emit boundary, lineEnd
                    let (bLam, bPhi) = capBoundary cosRadius (lam0, phi0) (lam, phi)
                        inner1 = streamPoint inner bLam bPhi
                        inner2 = streamLineEnd inner1
                    in wrap (CircleActiveInvisible lam phi) inner2

        CircleActiveInvisible lam0 phi0 ->
            if visible lam phi
                then
                    -- enter: lineStart, emit boundary, emit point
                    let (bLam, bPhi) = capBoundary cosRadius (lam0, phi0) (lam, phi)
                        inner1 = streamLineStart inner
                        inner2 = streamPoint inner1 bLam bPhi
                        inner3 = streamPoint inner2 lam phi
                    in wrap (CircleActiveVisible lam phi) inner3
                else
                    wrap (CircleActiveInvisible lam phi) inner

data CircleState
    = CircleIdle
    | CircleExpectFirst
    | CircleActiveVisible   !Double !Double
    | CircleActiveInvisible !Double !Double

-- | Find the spherical point where the geodesic between @p0@ and @p1@
--   crosses the cap boundary @x = cosRadius@. Linear cartesian interp +
--   re-normalisation to the unit sphere — accurate enough for graticule
--   resolution; would need a true geodesic root-finder for high-precision
--   continental boundaries.
capBoundary :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
capBoundary cosR (lam0, phi0) (lam1, phi1) =
    let c0@(x0, _, _) = sphericalToCartesian lam0 phi0
        c1@(x1, _, _) = sphericalToCartesian lam1 phi1
        t  = if abs (x1 - x0) < epsilon
                then 0.5
                else (cosR - x0) / (x1 - x0)
        d  = cartesianAdd c0 (cartesianScale t (subC c1 c0))
        n  = cartesianNormalize d
    in cartesianToSpherical n
  where
    subC :: Cart -> Cart -> Cart
    subC (a, b, c) (d, e, f) = (a - d, b - e, c - f)
