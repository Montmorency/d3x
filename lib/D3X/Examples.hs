{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module D3X.Examples where

import D3X.Prelude
import D3X.Blocks
import D3X.Scales
import D3X.Geo
    ( FeatureCollection(..), Feature, GeoObject(..)
    , decodeFeatureProperties
    , geoIdentity, reflectY, fitSize
    , geoPath, Projection
    )
import IHP.HSX.MarkupQQ (hsx)
import IHP.HSX.Markup (Html)
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Calendar (Day, diffDays, addDays)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.String.Conversions (cs)

-- Sparkline dimensions
sparkW, sparkH :: Int
sparkW = 500
sparkH = 150

sparkPadL, sparkPadR, sparkPadT, sparkPadB :: Double
sparkPadL = 30
sparkPadR = 10
sparkPadT = 15
sparkPadB = 25

labelSize :: Text
labelSize = "9"

-- | Render a price sparkline over time.
--   Takes a list of (day, price) pairs.
renderPriceSparkline :: [(Day, Double)] -> Html
renderPriceSparkline [] = [hsx|<span style="color:#999; font-size:0.78rem;">no data</span>|]
renderPriceSparkline pts = [hsx|
    <svg viewBox={vb} width={tshow sparkW} height={tshow sparkH}>
        <g>
            {axisLines}
            {axisLabels}
            {forEach pts renderPriceDot}
        </g>
    </svg>
    |]
  where
    vb = "0 0 " <> tshow sparkW <> " " <> tshow sparkH

    allDays   = map fst pts
    allPrices = map snd pts
    minDay = minimum allDays
    maxDay = maximum allDays
    dayDomain = if diffDays maxDay minDay == 0
        then Domain (addDays (-1) minDay, addDays 1 maxDay)
        else Domain (minDay, maxDay)
    minPrice = minimum allPrices
    maxPrice = maximum allPrices
    priceDomain = Domain (minPrice, maxPrice)

    xLeft   = Length sparkPadL
    xRight  = Length (fromIntegral sparkW - sparkPadR)
    yBottom = Length (fromIntegral sparkH - sparkPadB)
    yTop    = Length sparkPadT

    xScale :: Day -> Coordinate
    xScale = linearScale dayDomain (Range (xLeft, xRight))

    yScale :: Double -> Coordinate
    yScale = linearScale priceDomain (Range (yBottom, yTop))

    axisAttrs = defaultLine { stroke = "#ccc", strokeWidth = 1.0 }

    axisLines = [hsx|
        {xAxis axisAttrs xLeft xRight yBottom}
        {yAxis axisAttrs xLeft yBottom yTop}
    |]

    startLabel = formatDayCompact minDay
    endLabel   = formatDayCompact maxDay
    midDay     = addDays (diffDays maxDay minDay `div` 2) minDay
    midLabel   = formatDayCompact midDay
    midPrice   = (minPrice + maxPrice) / 2.0

    axisLabels = [hsx|
        {svgLabel xLeft (Length (fromIntegral sparkH - 2.0)) "start" labelSize startLabel}
        {svgLabel (xScale midDay) (Length (fromIntegral sparkH - 2.0)) "middle" labelSize midLabel}
        {svgLabel xRight (Length (fromIntegral sparkH - 2.0)) "end" labelSize endLabel}
        {svgLabel (Length 2) yTop "start" labelSize (formatPrice maxPrice)}
        {svgLabel (Length 2) (yScale midPrice) "start" labelSize (formatPrice midPrice)}
        {svgLabel (Length 2) yBottom "start" labelSize (formatPrice minPrice)}
    |]

    renderPriceDot (day, price) =
        let label = formatDayCompact day <> ": " <> formatPrice price
            attrs = defaultDot { fill = "steelblue", radius = 3.0 }
        in dotWithTitle attrs (xScale day) (yScale price) label

formatDayCompact :: Day -> Text
formatDayCompact = cs . formatTime defaultTimeLocale "%d/%m"

formatPrice :: Double -> Text
formatPrice p = tshow (fromIntegral (round (p * 100)) / 100.0 :: Double)

------------------------------------------------------------------------
-- Ireland counties (showcase example)
------------------------------------------------------------------------

-- | Decoded shape of the @"NAME"@ field on each Ireland county feature.
newtype CountyName = CountyName { unCountyName :: Text }

instance FromJSON CountyName where
    parseJSON = withObject "CountyName" $ \o -> CountyName <$> o .: "NAME"

-- | Render an Irish-counties 'FeatureCollection' as a full-width SVG map,
--   fitted to a 960×600 viewBox via 'geoIdentity' + 'reflectY' (the
--   data is in Irish National Grid easting/northing).
--
--   Each county's @\<title\>@ shows its name on hover. If the county's
--   @"NAME"@ is a key in the @actions@ map, the path is wired with
--   @hx-get=URL@ — clicking triggers the htmx request inheriting any
--   ancestor @hx-target@. Counties absent from the map render inert.
renderIrelandCounties
    :: FeatureCollection
    -> Map Text Text   -- ^ @countyName -> URL@ for htmx click actions
    -> Html
renderIrelandCounties fc actions =
    let proj = geoIdentity & reflectY True & fitSize (mapW, mapH) (GoFeatureCollection fc)
        viewBox = "0 0 " <> tshow mapW <> " " <> tshow mapH
    in [hsx|
        <svg viewBox={viewBox} width="100%" style="display:block;">
            {forEach (fcFeatures fc) (renderCounty proj actions)}
        </svg>
    |]
  where
    mapW, mapH :: Double
    mapW = 960
    mapH = 600

renderCounty :: Projection -> Map Text Text -> Feature -> Html
renderCounty proj actions f =
    let d    = geoPath proj (GoFeature f)
        name = case decodeFeatureProperties f of
                   Right (CountyName n) -> n
                   Left _               -> ""
        clickAttrs = case Map.lookup name actions of
            Just url -> [("hx-get", url), ("style", "cursor:pointer; transition: fill 0.15s ease;")]
            Nothing  -> [("style", "transition: fill 0.15s ease;")]
    in [hsx|
        <path d={d}
              fill="#cfd8dc" stroke="#ffffff" stroke-width="0.7"
              {...clickAttrs}
              onmouseover="this.style.fill='#90a4ae'"
              onmouseout="this.style.fill='#cfd8dc'">
            <title>{name}</title>
        </path>
    |]
