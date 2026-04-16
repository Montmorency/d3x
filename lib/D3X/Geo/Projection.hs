{-# LANGUAGE ImplicitPrelude #-}
-- | The 'Projection' record + the @projectionStream@ pipeline that
--   composes pre-projection rotation, clipping, raw projection, the
--   outer affine (scale / translate / reflect), resampling and
--   post-clipping into a single @'Stream' r -> 'Stream' r@ transformer.
--
--   Mirrors @d3-geo/src/projection/index.js@ (the @projectionMutator@
--   factory) and @src/projection/identity.js@.
module D3X.Geo.Projection
    ( -- * Type
      Projection(..)
    , ProjKind(..)
      -- * Constructors
    , geoIdentity
    , geoMercator
    , geoTransverseMercator
    , geoConicConformal
    , geoOrthographic
      -- * Setters (object-last, compose with @&@)
    , scale
    , translate
    , center
    , rotate
    , angle
    , reflectX
    , reflectY
    , precision
    , preclip
    , postclip
      -- * Pipeline
    , projectionStream
    , transformRadians
    ) where

import D3X.Geo.Stream (Stream, mapPoint)
import D3X.Geo.Raw
    ( RawProjection(..), identityRaw
    , mercatorRaw, transverseMercatorRaw, conicConformalRaw, orthographicRaw )
import D3X.Geo.Math (radians)
import D3X.Geo.Clip (PreclipKind(..), PostclipKind(..), interpretPreclip, interpretPostclip)
import D3X.Geo.Rotation (rotateRadians)
import D3X.Geo.Resample (resampleNone, resample)

-- | Whether the projection is planar (e.g. 'geoIdentity', taking
--   already-projected Cartesian coordinates) or spherical (taking
--   degrees of longitude/latitude as input).
data ProjKind
    = Planar
    | Spherical
    deriving (Eq, Show)

-- | A complete projection: raw mathematical projection plus the outer
--   affine transform plus optional spherical preprocessing.
--
--   Modify with the setter functions (@scale@, @translate@, ...) which
--   are object-last so they compose with @'&'@:
--
-- @
-- 'geoIdentity' & 'reflectY' True & 'D3X.Geo.Fit.fitSize' (960, 600) ireland
-- @
data Projection = Projection
    { projKind      :: !ProjKind
    , projRaw       :: !RawProjection
    , projScale     :: !Double                       -- ^ k
    , projTranslate :: !(Double, Double)             -- ^ (tx, ty)
    , projCenter    :: !(Double, Double)             -- ^ (lambda, phi) radians
    , projRotate    :: !(Double, Double, Double)     -- ^ Euler radians
    , projAngle     :: !Double                       -- ^ post-projection rotation, radians (Phase B)
    , projReflectX  :: !Bool
    , projReflectY  :: !Bool
    , projPrecision :: !Double                       -- ^ delta squared for resample
    , projPreclip   :: !PreclipKind
    , projPostclip  :: !PostclipKind
    }

------------------------------------------------------------------------
-- Constructors
------------------------------------------------------------------------

-- | The planar identity projection (mirrors @d3.geoIdentity()@).
--   Coordinates pass through unchanged except for the outer affine
--   (reflect + scale + translate). Use with already-projected data
--   such as the Irish National Grid eastings/northings.
geoIdentity :: Projection
geoIdentity = Projection
    { projKind      = Planar
    , projRaw       = identityRaw
    , projScale     = 1
    , projTranslate = (0, 0)
    , projCenter    = (0, 0)
    , projRotate    = (0, 0, 0)
    , projAngle     = 0
    , projReflectX  = False
    , projReflectY  = False
    , projPrecision = 0
    , projPreclip   = ClipNone
    , projPostclip  = ClipIdentity
    }

-- | Spherical-projection defaults: scale 150, translate (480, 250),
--   precision 0.5 px (delta² = 0.25), antimeridian preclip, identity
--   postclip. Used by 'geoMercator', 'geoTransverseMercator',
--   'geoConicConformal'.
sphericalDefaults :: RawProjection -> Projection
sphericalDefaults raw = Projection
    { projKind      = Spherical
    , projRaw       = raw
    , projScale     = 150
    , projTranslate = (480, 250)
    , projCenter    = (0, 0)
    , projRotate    = (0, 0, 0)
    , projAngle     = 0
    , projReflectX  = False
    , projReflectY  = False
    , projPrecision = 0.25                -- delta² = (0.5 px)²
    , projPreclip   = ClipAntimeridian
    , projPostclip  = ClipIdentity
    }

-- | The Mercator projection. Conformal cylindrical; meridians are
--   straight vertical lines, parallels are horizontal but spaced wider
--   apart toward the poles.
geoMercator :: Projection
geoMercator = sphericalDefaults mercatorRaw

-- | Mercator rotated 90°; minimal distortion in narrow longitude bands
--   (UTM zones, Irish Grid). Default rotation @(0, 0, -90°)@ to put
--   the central meridian along the y-axis.
geoTransverseMercator :: Projection
geoTransverseMercator = (sphericalDefaults transverseMercatorRaw)
    { projRotate = (0, 0, - pi / 2) }

-- | Lambert Conformal Conic, parameterised by two standard parallels.
--   Default parallels are 29.5° and 45.5° (USA convention).
geoConicConformal :: Projection
geoConicConformal = sphericalDefaults
    (conicConformalRaw (radians 29.5) (radians 45.5))

-- | Orthographic perspective (the "globe seen from space"). Hemisphere
--   clip is enabled by default so the back side does not leak through.
geoOrthographic :: Projection
geoOrthographic = (sphericalDefaults orthographicRaw)
    { projScale     = 250
    , projPreclip   = ClipCircle (pi / 2)
    }

------------------------------------------------------------------------
-- Setters (object-last)
------------------------------------------------------------------------

scale       :: Double                  -> Projection -> Projection
scale k p     = p { projScale = k }

translate   :: (Double, Double)        -> Projection -> Projection
translate t p = p { projTranslate = t }

-- | Center: arguments in __degrees__, stored internally as radians.
center      :: (Double, Double)        -> Projection -> Projection
center (lam, phi) p = p { projCenter = (radians lam, radians phi) }

-- | Rotate: arguments in __degrees__, stored internally as radians.
rotate      :: (Double, Double, Double) -> Projection -> Projection
rotate (a, b, c) p = p { projRotate = (radians a, radians b, radians c) }

-- | Post-projection rotation in pixel space. Argument in __degrees__,
--   stored as radians. Phase B.
angle       :: Double                  -> Projection -> Projection
angle a p     = p { projAngle = radians a }

reflectX    :: Bool                    -> Projection -> Projection
reflectX b p  = p { projReflectX = b }

reflectY    :: Bool                    -> Projection -> Projection
reflectY b p  = p { projReflectY = b }

precision   :: Double                  -> Projection -> Projection
precision d p = p { projPrecision = d * d }

preclip     :: PreclipKind             -> Projection -> Projection
preclip pk p  = p { projPreclip = pk }

postclip    :: PostclipKind            -> Projection -> Projection
postclip pc p = p { projPostclip = pc }

------------------------------------------------------------------------
-- The pipeline
------------------------------------------------------------------------

-- | Convert each emitted point from degrees to radians. Used as the
--   first stage of the spherical pipeline; bypassed entirely by 'Planar'.
transformRadians :: Stream r -> Stream r
transformRadians = mapPoint (\x y -> (radians x, radians y))

-- | Apply the outer affine — reflect, scale, translate — to each point
--   after the raw projection. Phase A's 'angle' field is ignored here
--   (rotation in pixel space lands in Phase B).
affine :: Projection -> Stream r -> Stream r
affine p = mapPoint $ \x y ->
    let xr = if projReflectX p then -x else x
        yr = if projReflectY p then -y else y
        (tx, ty) = projTranslate p
        k        = projScale p
    in (k * xr + tx, k * yr + ty)

-- | Compose the full projection pipeline into a single stream transformer.
--   For 'Planar' projections (e.g. 'geoIdentity') the spherical prefix
--   is skipped entirely; for 'Spherical' projections the full d3-geo
--   pipeline is assembled.
projectionStream :: Projection -> Stream r -> Stream r
projectionStream p = case projKind p of
    Planar ->
          interpretPreclip  (projPreclip  p)   -- Phase A: id; included so the seam exists
        . projectRaw                            -- raw projection (identity for geoIdentity)
        . affine p                              -- reflect + scale + translate
        . interpretPostclip (projPostclip p)
    Spherical ->
          transformRadians
        . rotateRadians   (projRotate p)
        . interpretPreclip (projPreclip p)
        . projectResample                       -- raw projection + adaptive subdivision
        . affine p
        . interpretPostclip (projPostclip p)
  where
    -- Apply the raw projection point-by-point, no subdivision (Phase A).
    projectRaw      = resampleNone (projRaw p)
    projectResample = resample     (projRaw p) (projPrecision p)
