{-# LANGUAGE ImplicitPrelude #-}
-- | A 'Stream' is a record-of-functions consumer of geometry events,
--   parameterised by the result type that the consumer accumulates.
--
--   This mirrors d3-geo's stream protocol (@src/stream.js@,
--   @src/transform.js@) but explicitly threads state by returning a fresh
--   @Stream r@ from every method call. Transformers are plain
--   @Stream r -> Stream r@ functions; the projection pipeline is built
--   with ordinary function composition.
module D3X.Geo.Stream
    ( -- * Stream type
      Stream(..)
    , runStream
      -- * GeoJSON driver
    , geoStream
    , geoStreamGeometry
      -- * Transformer combinators
    , identityXf
    , mapPoint
    ) where

import D3X.Geo.GeoJSON
    ( Position(..), Geometry(..), Feature(..), FeatureCollection(..), GeoObject(..) )

-- | A streaming consumer. @r@ is the accumulator type that
--   'streamResult' exposes after all events have been delivered.
--
--   Each method returns a *new* @Stream r@ that captures any updated
--   internal state.
data Stream r = Stream
    { streamPoint        :: Double -> Double -> Stream r
    , streamLineStart    :: Stream r
    , streamLineEnd      :: Stream r
    , streamPolygonStart :: Stream r
    , streamPolygonEnd   :: Stream r
    , streamSphere       :: Stream r
    , streamResult       :: r
    }

-- | Drive a 'GeoObject' through a stream and read the result.
runStream :: GeoObject -> Stream r -> r
runStream o = streamResult . geoStream o

-- | Walk a 'GeoObject', emitting events into the stream.
geoStream :: GeoObject -> Stream r -> Stream r
geoStream o s = case o of
    GoFeature f -> case featGeometry f of
        Just g  -> geoStreamGeometry g s
        Nothing -> s
    GoFeatureCollection (FeatureCollection fs) ->
        foldl (\acc f -> geoStream (GoFeature f) acc) s fs
    GoGeometry g -> geoStreamGeometry g s
    GoGeometryCollection gs ->
        foldl (\acc g -> geoStreamGeometry g acc) s gs

-- | Walk a single 'Geometry' through the stream protocol.
--   Mirrors @d3-geo/src/stream.js@'s dispatch table.
geoStreamGeometry :: Geometry -> Stream r -> Stream r
geoStreamGeometry g s = case g of
    Point (Position x y)    -> streamPoint s x y
    MultiPoint ps           -> foldl emitPoint s ps
    LineString ps           -> emitLine ps s
    MultiLineString lss     -> foldl (flip emitLine) s lss
    Polygon rings           -> emitPolygon rings s
    MultiPolygon polys      -> foldl (flip emitPolygon) s polys
    GeometryCollection gs   -> foldl (flip geoStreamGeometry) s gs
    Sphere                  -> streamSphere s

emitPoint :: Stream r -> Position -> Stream r
emitPoint s (Position x y) = streamPoint s x y

emitLine :: [Position] -> Stream r -> Stream r
emitLine ps s =
    let s1 = streamLineStart s
        s2 = foldl emitPoint s1 ps
    in streamLineEnd s2

emitPolygon :: [[Position]] -> Stream r -> Stream r
emitPolygon rings s =
    let s1 = streamPolygonStart s
        s2 = foldl (flip emitLine) s1 rings
    in streamPolygonEnd s2

------------------------------------------------------------------------
-- Transformer combinators
------------------------------------------------------------------------

-- | The identity transformer (no-op). Forwards every event unchanged.
identityXf :: Stream r -> Stream r
identityXf = id

-- | Wrap a stream so that every emitted point is first mapped through @f@.
--   This is the single building block used by Phase A's @transformRadians@,
--   @transformRotate@, projection-application, @reflectY@, etc.
--
--   Mirrors @d3-geo/src/transform.js@'s point-only transformer.
mapPoint :: (Double -> Double -> (Double, Double)) -> Stream r -> Stream r
mapPoint f = adapt
  where
    adapt s = Stream
        { streamPoint = \x y ->
            let (x', y') = f x y
            in adapt (streamPoint s x' y')
        , streamLineStart    = adapt (streamLineStart s)
        , streamLineEnd      = adapt (streamLineEnd s)
        , streamPolygonStart = adapt (streamPolygonStart s)
        , streamPolygonEnd   = adapt (streamPolygonEnd s)
        , streamSphere       = adapt (streamSphere s)
        , streamResult       = streamResult s
        }
