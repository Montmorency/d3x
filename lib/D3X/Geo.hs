{-# LANGUAGE ImplicitPrelude #-}
-- | Umbrella re-export for the @D3X.Geo.*@ subsystem.
--
--   Lifts a minimal subset of @d3-geo@ into Haskell: a stream-based
--   GeoJSON pipeline, a 'Projection' record with d3-style setters,
--   @fit@-helpers, and an SVG path renderer.
--
--   __Phase A__ ships only 'geoIdentity' (the planar pass-through
--   projection used by already-projected data such as the Irish
--   National Grid). Phase B adds Mercator, Transverse Mercator,
--   Conic Conformal plus real adaptive resampling and antimeridian
--   clipping.
module D3X.Geo
    ( module D3X.Geo.GeoJSON
    , module D3X.Geo.Projection
    , module D3X.Geo.Fit
    , module D3X.Geo.Path
    , module D3X.Geo.Clip
    , module D3X.Geo.Stream
    , module D3X.Geo.Graticule
    ) where

import D3X.Geo.GeoJSON
import D3X.Geo.Projection
import D3X.Geo.Fit
import D3X.Geo.Path
import D3X.Geo.Clip
import D3X.Geo.Stream
import D3X.Geo.Graticule
