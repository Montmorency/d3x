{-# LANGUAGE ImplicitPrelude #-}
-- | Raw, unframed projection functions and their inverses.
--
--   A 'RawProjection' takes spherical coordinates in __radians__ (or
--   native Cartesian for 'identityRaw') and returns Cartesian @(x, y)@
--   in unit space, before the outer scale + translate is applied by
--   'D3X.Geo.Projection'.
module D3X.Geo.Raw
    ( RawProjection(..)
    , identityRaw
      -- * Cylindrical
    , mercatorRaw
    , transverseMercatorRaw
      -- * Conic
    , conicConformalRaw
      -- * Azimuthal
    , orthographicRaw
    ) where

import D3X.Geo.Math (halfPi, epsilon)

data RawProjection = RawProjection
    { rawProject :: Double -> Double -> (Double, Double)
    , rawInvert  :: Maybe ((Double, Double) -> (Double, Double))
    }

-- | Pass coordinates straight through. Used by 'D3X.Geo.Projection.geoIdentity'.
identityRaw :: RawProjection
identityRaw = RawProjection
    { rawProject = (,)
    , rawInvert  = Just id
    }

------------------------------------------------------------------------
-- Cylindrical
------------------------------------------------------------------------

-- | Mercator: cylindrical conformal. Diverges at the poles.
mercatorRaw :: RawProjection
mercatorRaw = RawProjection
    { rawProject = \lambda phi -> (lambda, log (tan ((halfPi + phi) / 2)))
    , rawInvert  = Just (\(x, y) -> (x, 2 * atan (exp y) - halfPi))
    }

-- | Transverse Mercator: Mercator rotated 90°, swapping the role of
--   meridian and equator. Tight in narrow longitude bands (Irish Grid,
--   UTM zones, …).
transverseMercatorRaw :: RawProjection
transverseMercatorRaw = RawProjection
    { rawProject = \lambda phi -> (log (tan ((halfPi + phi) / 2)), -lambda)
    , rawInvert  = Just (\(x, y) -> (-y, 2 * atan (exp x) - halfPi))
    }

------------------------------------------------------------------------
-- Conic
------------------------------------------------------------------------

-- | Lambert Conformal Conic, parameterised by two standard parallels
--   (in radians). Excellent for mid-latitude regions; degenerates to
--   Mercator when both parallels collapse to zero.
conicConformalRaw :: Double -> Double -> RawProjection
conicConformalRaw y0 y1
    | abs n < epsilon = mercatorRaw
    | otherwise = RawProjection
        { rawProject = project
        , rawInvert  = Just invert
        }
  where
    tany y = tan ((halfPi + y) / 2)
    cy0    = cos y0
    n      = if y0 == y1
                then sin y0
                else log (cy0 / cos y1) / log (tany y1 / tany y0)
    f      = cy0 * (tany y0 ** n) / n

    clamp y
        | f > 0 && y < -halfPi + epsilon = -halfPi + epsilon
        | f < 0 && y >  halfPi - epsilon =  halfPi - epsilon
        | otherwise = y

    project lambda phi =
        let y = clamp phi
            r = f / (tany y ** n)
        in (r * sin (n * lambda), f - r * cos (n * lambda))

    invert (x, y) =
        let fy = f - y
            r  = signum n * sqrt (x * x + fy * fy)
            l0 = atan2 x (abs fy) * signum fy
            l  = if fy * n < 0
                    then l0 - pi * signum x * signum fy
                    else l0
        in (l / n, 2 * atan ((f / r) ** (1 / n)) - halfPi)

------------------------------------------------------------------------
-- Azimuthal
------------------------------------------------------------------------

-- | Orthographic: azimuthal perspective from infinite distance —
--   the classic "globe seen from space". Only the visible hemisphere
--   is meaningful; pair with 'D3X.Geo.Clip.ClipCircle' (radius @halfPi@)
--   to hide the back side.
orthographicRaw :: RawProjection
orthographicRaw = RawProjection
    { rawProject = \lambda phi -> (cos phi * sin lambda, sin phi)
    , rawInvert  = Just invert
    }
  where
    invert (x, y) =
        let z2 = 1 - x*x - y*y
            z  = sqrt (max 0 z2)
            sc = z
            cc = sqrt (max 0 (1 - z*z))
        in if z < epsilon
                then (0, 0)   -- outside hemisphere; caller should clipCircle
                else (atan2 (x * sc) (z * cc), asin (if z == 0 then 0 else y * sc / z))
