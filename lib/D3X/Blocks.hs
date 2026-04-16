{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module D3X.Blocks where

import D3X.Prelude
import D3X.Scales (Coordinate, showCoord)
import IHP.HSX.MarkupQQ (hsx)
import IHP.HSX.Markup (Html, ToHtml(..))

-- | Convert an attrs record to [(Text, Text)] for HSX spread syntax
class SpreadAttrs a where
  spreadAttrs :: a -> [(Text, Text)]

-- | Attributes for circle (dot) elements.
--   Use record update as "spread":
--
-- @
-- dot defaultDot { fill = "green", radius = 6 } cx cy
-- @
data DotAttrs = DotAttrs
  { radius :: !Double
  , fill   :: !Text
  }

defaultDot :: DotAttrs
defaultDot = DotAttrs { radius = 8, fill = "steelblue" }

instance SpreadAttrs DotAttrs where
  spreadAttrs DotAttrs{..} =
    [ ("r", tshow radius)
    , ("fill", fill)
    ]

-- | Attributes for line (path) elements:
--
-- @
-- line dashedLine { stroke = "blue" } (x1,y1) (x2,y2)
-- @
data LineAttrs = LineAttrs
  { stroke          :: !Text
  , strokeWidth     :: !Double
  , strokeDasharray :: !Text
  }

defaultLine :: LineAttrs
defaultLine = LineAttrs { stroke = "currentColor", strokeWidth = 1, strokeDasharray = "" }

dashedLine :: LineAttrs
dashedLine = LineAttrs { stroke = "red", strokeWidth = 1, strokeDasharray = "3 1" }

instance SpreadAttrs LineAttrs where
  spreadAttrs LineAttrs{..} =
    [ ("stroke", stroke)
    , ("stroke-width", tshow strokeWidth)
    , ("stroke-dasharray", strokeDasharray)
    ]

-- | Render a circle at scaled coordinates, spreading dotAttrs
dot :: DotAttrs -> Coordinate -> Coordinate -> Html
dot dotAttrs cx cy =
  let attrs = spreadAttrs dotAttrs
      cxT = showCoord cx
      cyT = showCoord cy
  in [hsx|<circle cx={cxT} cy={cyT} {...attrs}/>|]

-- | Render a circle with a tooltip, spreading dotAttrs
dotWithTitle :: DotAttrs -> Coordinate -> Coordinate -> Text -> Html
dotWithTitle dotAttrs cx cy titleText =
  let attrs = spreadAttrs dotAttrs
      cxT = showCoord cx
      cyT = showCoord cy
  in [hsx|<circle cx={cxT} cy={cyT} {...attrs}><title>{titleText}</title></circle>|]

-- | Render a straight line between two scaled points, spreading lineAttrs
line :: LineAttrs -> (Coordinate, Coordinate) -> (Coordinate, Coordinate) -> Html
line lineAttrs (x1, y1) (x2, y2) =
  let attrs = spreadAttrs lineAttrs
      d = "M" <> showCoord x1 <> "," <> showCoord y1
       <> "L" <> showCoord x2 <> "," <> showCoord y2
  in [hsx|<path d={d} fill="none" {...attrs}/>|]

-- | Render a horizontal axis line (x-axis) at a given y position
xAxis :: LineAttrs -> Coordinate -> Coordinate -> Coordinate -> Html
xAxis attrs xLeft xRight y = line attrs (xLeft, y) (xRight, y)

-- | Render a vertical axis line (y-axis) at a given x position
yAxis :: LineAttrs -> Coordinate -> Coordinate -> Coordinate -> Html
yAxis attrs x yBottom yTop = line attrs (x, yBottom) (x, yTop)

-- | Render an SVG text label
svgLabel :: Coordinate -> Coordinate -> Text -> Text -> Text -> Html
svgLabel x y anchor fontSize label =
  let xT = showCoord x
      yT = showCoord y
  in [hsx|<text x={xT} y={yT} text-anchor={anchor} font-size={fontSize} fill="currentColor" opacity="0.5">{label}</text>|]

-- | Wrap SVG content with a viewBox
svgViewBox :: Int -> Int -> Html -> Html
svgViewBox w h body =
  let vb = "0 0 " <> tshow w <> " " <> tshow h
      wt = tshow w
      ht = tshow h
  in [hsx|<svg viewBox={vb} width={wt} height={ht}>{body}</svg>|]
