module D3X.Examples where

import D3X.Prelude
import D3X.Blocks
import D3X.Scales
import IHP.HSX.MarkupQQ (hsx)
import IHP.HSX.Markup (Html)
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
            {map renderPriceDot pts}
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
