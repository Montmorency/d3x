{-# LANGUAGE ImplicitPrelude #-}
-- | Procedurally generate lat/lon grid lines as a 'MultiLineString'
--   geometry — the canonical d3-geo demo for any spherical projection.
--
--   Mirrors @d3-geo/src/graticule.js@ but exposes a flat parameter list
--   instead of d3-geo's chained-setter API.
module D3X.Geo.Graticule
    ( -- * Default
      graticule
      -- * Custom
    , GraticuleOpts(..)
    , defaultGraticuleOpts
    , graticuleWith
    ) where

import D3X.Geo.GeoJSON (Geometry(..), Position(..))

data GraticuleOpts = GraticuleOpts
    { gStep      :: !Double          -- ^ degrees between grid lines
    , gLatExtent :: !(Double, Double)  -- ^ (latMin, latMax) in degrees
    , gLonExtent :: !(Double, Double)  -- ^ (lonMin, lonMax) in degrees
    , gSampling  :: !Double          -- ^ degrees between intermediate points within each line
    } deriving (Eq, Show)

-- | Sensible default: 10° grid, latitudes capped at ±80° to avoid the
--   Mercator polar singularity, 2.5° intermediate sampling.
defaultGraticuleOpts :: GraticuleOpts
defaultGraticuleOpts = GraticuleOpts
    { gStep      = 10
    , gLatExtent = (-80, 80)
    , gLonExtent = (-180, 180)
    , gSampling  = 2.5
    }

-- | The standard graticule: 10° grid lines from -180° to +180°
--   longitude and -80° to +80° latitude. Use with any spherical
--   projection ('geoMercator', 'geoOrthographic', …).
graticule :: Geometry
graticule = graticuleWith defaultGraticuleOpts

graticuleWith :: GraticuleOpts -> Geometry
graticuleWith opts = MultiLineString (meridians ++ parallels)
  where
    step           = gStep      opts
    (lat0, lat1)   = gLatExtent opts
    (lon0, lon1)   = gLonExtent opts
    sampling       = gSampling  opts

    -- meridians: lines of constant longitude
    meridians =
        [ [ Position lon lat | lat <- samples lat0 lat1 sampling ]
        | lon <- samples lon0 lon1 step
        ]

    -- parallels: lines of constant latitude
    parallels =
        [ [ Position lon lat | lon <- samples lon0 lon1 sampling ]
        | lat <- samples lat0 lat1 step
        ]

-- | Inclusive samples from @a@ to @b@ at step @s@. Always includes the
--   endpoints (@b@ is appended if it isn't already hit by the arithmetic).
samples :: Double -> Double -> Double -> [Double]
samples a b s
    | s <= 0   = [a, b]
    | a > b    = []
    | otherwise = go a
  where
    go x
        | x >= b - 1e-9 = [b]
        | otherwise     = x : go (x + s)
