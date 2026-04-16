{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- | The 'PathStream' consumer turns geometry events into an SVG @d@
--   string by driving 'D3X.Path' constructors. Plus the user-facing
--   'geoPath' / 'geoPathHtml' / 'geoPathFeatures' wrappers.
--
--   Mirrors @d3-geo/src/path/string.js@.
module D3X.Geo.Path
    ( -- * The consumer
      pathStream
      -- * User API
    , geoPath
    , geoPathHtml
    , geoPathHtmlWith
    , geoPathFeatures
    ) where

import Control.Monad.State.Strict (execState)
import IHP.HSX.MarkupQQ (hsx)
import IHP.HSX.Markup (Html)

import D3X.Prelude (Text, forEach)
import qualified D3X.Path as DP
import D3X.Path (PathBuilder, PathState, PathStr)
import D3X.Geo.GeoJSON
    ( Feature, FeatureCollection(..), GeoObject(..) )
import D3X.Geo.Projection (Projection, projectionStream)
import D3X.Geo.Stream (Stream(..), geoStream)

------------------------------------------------------------------------
-- State machine (mirrors d3-geo's PathString._line / _point flags)
------------------------------------------------------------------------

data PointMode
    = ExpectMove   -- ^ next point starts a new sub-path with M
    | ExpectLine   -- ^ next point continues the sub-path with L
    | NoLine       -- ^ next point is orphan; emit a circle

------------------------------------------------------------------------
-- The Stream consumer
------------------------------------------------------------------------

-- | A fresh 'Stream' that accumulates an SVG @d@ string.
--   Polygon rings are closed with @Z@; orphan points are emitted as
--   small circles of radius 4.5.
pathStream :: Stream PathStr
pathStream = mk DP.emptyPathState False NoLine 4.5

mk :: PathState -> Bool -> PointMode -> Double -> Stream PathStr
mk st inPoly pm radius = Stream
    { streamPoint = \x y -> case pm of
        ExpectMove -> mk (runPB (DP.moveTo (DP.M (x, y))) st) inPoly ExpectLine radius
        ExpectLine -> mk (runPB (DP.lineTo (DP.L (x, y))) st) inPoly ExpectLine radius
        NoLine     -> mk (runPB (DP.canvasArc x y radius 0 (2 * pi) False) st)
                         inPoly NoLine radius
    , streamLineStart    = mk st inPoly ExpectMove radius
    , streamLineEnd      = if inPoly
        then mk (runPB (DP.closePath DP.Z) st) inPoly NoLine radius
        else mk st inPoly NoLine radius
    , streamPolygonStart = mk st True  pm radius
    , streamPolygonEnd   = mk st False pm radius
    , streamSphere       = mk st inPoly pm radius
    , streamResult       = DP.psPath st
    }

runPB :: PathBuilder a -> PathState -> PathState
runPB pb = execState pb

------------------------------------------------------------------------
-- User API
------------------------------------------------------------------------

-- | Render a 'GeoObject' through a projection into an SVG @d@ string.
geoPath :: Projection -> GeoObject -> Text
geoPath proj obj =
    let consumer = projectionStream proj pathStream
    in streamResult (geoStream obj consumer)

-- | Render a 'GeoObject' as a self-closing @\<path\>@ element with the
--   given attributes.
geoPathHtml :: Projection -> [(Text, Text)] -> GeoObject -> Html
geoPathHtml proj attrs obj =
    let d = geoPath proj obj
    in [hsx|<path d={d} {...attrs}/>|]

-- | Like 'geoPathHtml' but emits children inside the path element
--   — typically a @\<title\>@ for hover tooltips.
geoPathHtmlWith :: Projection -> [(Text, Text)] -> Html -> GeoObject -> Html
geoPathHtmlWith proj attrs children obj =
    let d = geoPath proj obj
    in [hsx|<path d={d} {...attrs}>{children}</path>|]

-- | Render every 'Feature' in a 'FeatureCollection' as its own
--   @\<path\>@ element. The @toAttrs@ callback decides per-feature
--   attributes (fill, stroke, classes, …).
--
--   Uses 'forEach' so that the resulting 'Html' is a single
--   markup-monoid concatenation (no @[Html]@ stringification).
geoPathFeatures
    :: Projection
    -> (Feature -> [(Text, Text)])
    -> FeatureCollection
    -> Html
geoPathFeatures proj toAttrs (FeatureCollection fs) =
    forEach fs (\f -> geoPathHtml proj (toAttrs f) (GoFeature f))
