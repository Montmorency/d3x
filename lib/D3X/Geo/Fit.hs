{-# LANGUAGE ImplicitPrelude #-}
-- | Compute a projection's @scale@ and @translate@ so that a given
--   GeoJSON object fits inside a target rectangle.
--
--   Mirrors @d3-geo/src/projection/fit.js@.
module D3X.Geo.Fit
    ( fitExtent
    , fitSize
    , fitWidth
    , fitHeight
    ) where

import D3X.Geo.Bounds (boundsStream)
import D3X.Geo.Clip (PostclipKind(..))
import D3X.Geo.GeoJSON (GeoObject)
import D3X.Geo.Projection
    ( Projection(..), projectionStream )
import D3X.Geo.Stream (geoStream, streamResult)

-- | Set the projection's @scale@ and @translate@ so the geometry fits
--   inside the given extent rectangle (@((x0,y0),(x1,y1))@), centered.
fitExtent
    :: ((Double, Double), (Double, Double))   -- ^ target rectangle
    -> GeoObject                              -- ^ geometry to fit
    -> Projection                             -- ^ input projection
    -> Projection                             -- ^ updated projection
fitExtent ((x0, y0), (x1, y1)) obj p0 =
    let -- Phase A: precision is unused. Snapshot scale/translate/postclip,
        -- replace them with neutral values so the bounds we measure are in
        -- the projection's "unit space" (post-raw, pre-affine-by-1).
        tempProj = p0
            { projScale     = 1
            , projTranslate = (0, 0)
            , projPostclip  = ClipIdentity
            }
        consumer = projectionStream tempProj boundsStream
        bbox     = streamResult (geoStream obj consumer)
    in case bbox of
        Nothing -> p0
        Just ((b0x, b0y), (b1x, b1y)) ->
            let dx = b1x - b0x
                dy = b1y - b0y
                kx = if dx == 0 then 1/0 else (x1 - x0) / dx
                ky = if dy == 0 then 1/0 else (y1 - y0) / dy
                k  = min kx ky
                tx = (x0 + x1 - k * (b0x + b1x)) / 2
                ty = (y0 + y1 - k * (b0y + b1y)) / 2
            in p0 { projScale = k, projTranslate = (tx, ty) }

-- | Convenience: fit into a @(width, height)@ rectangle anchored at the origin.
fitSize :: (Double, Double) -> GeoObject -> Projection -> Projection
fitSize (w, h) = fitExtent ((0, 0), (w, h))

-- | Fit width; height is derived from the bounds aspect ratio.
fitWidth :: Double -> GeoObject -> Projection -> Projection
fitWidth w obj p0 =
    let tempProj = p0 { projScale = 1, projTranslate = (0, 0), projPostclip = ClipIdentity }
        bbox     = streamResult (geoStream obj (projectionStream tempProj boundsStream))
    in case bbox of
        Nothing -> p0
        Just ((b0x, b0y), (b1x, b1y)) ->
            let dx = b1x - b0x
                dy = b1y - b0y
                h  = if dx == 0 then 0 else w * dy / dx
            in fitExtent ((0, 0), (w, h)) obj p0

-- | Fit height; width is derived from the bounds aspect ratio.
fitHeight :: Double -> GeoObject -> Projection -> Projection
fitHeight h obj p0 =
    let tempProj = p0 { projScale = 1, projTranslate = (0, 0), projPostclip = ClipIdentity }
        bbox     = streamResult (geoStream obj (projectionStream tempProj boundsStream))
    in case bbox of
        Nothing -> p0
        Just ((b0x, b0y), (b1x, b1y)) ->
            let dx = b1x - b0x
                dy = b1y - b0y
                w  = if dy == 0 then 0 else h * dx / dy
            in fitExtent ((0, 0), (w, h)) obj p0
