{-# LANGUAGE ImplicitPrelude #-}
-- | A 'Stream' consumer that accumulates the axis-aligned bounding box
--   of every projected point it sees.
--
--   Used by 'D3X.Geo.Fit' to compute @scale@ and @translate@ for
--   @fitExtent@ / @fitSize@.
module D3X.Geo.Bounds
    ( Bounds
    , boundsStream
    , extendBounds
    ) where

import D3X.Geo.Stream (Stream(..))

-- | @Just ((minX, minY), (maxX, maxY))@ — or 'Nothing' if no points
--   have been seen yet.
type Bounds = Maybe ((Double, Double), (Double, Double))

extendBounds :: (Double, Double) -> Bounds -> Bounds
extendBounds (x, y) Nothing                          = Just ((x, y), (x, y))
extendBounds (x, y) (Just ((x0, y0), (x1, y1)))      =
    Just ((min x x0, min y y0), (max x x1, max y y1))

-- | A fresh 'Stream' that tracks the bounding box of all incoming
--   points. Non-point events (line/polygon/sphere markers) are no-ops.
boundsStream :: Stream Bounds
boundsStream = go Nothing
  where
    go b = Stream
        { streamPoint        = \x y -> go (extendBounds (x, y) b)
        , streamLineStart    = go b
        , streamLineEnd      = go b
        , streamPolygonStart = go b
        , streamPolygonEnd   = go b
        , streamSphere       = go b
        , streamResult       = b
        }
