module D3X.Path
    ( -- * Types
      Point
    , PathStr
    , PathState(..)
    , PathBuilder
      -- * Path Data Constructors (SVG path commands)
    , MoveTo(..)
    , LineTo(..)
    , ClosePath(..)
    , ArcTo(..)
      -- * Path Operations
    , moveTo
    , lineTo
    , closePath
    , arcTo
    , canvasArc
      -- * Running
    , buildPath
    , emptyPathState
    ) where

import D3X.Prelude
import Control.Monad (when)
import Control.Monad.State.Strict

-- | A point in 2D space
type Point = (Double, Double)

-- | SVG path data string
type PathStr = Text

-- | Internal state for the path builder monad
data PathState = PathState
    { psCursor   :: !Point   -- ^ Current drawing position
    , psPath     :: !PathStr -- ^ Accumulated SVG path data
    , psHasPoint :: !Bool    -- ^ Whether a subpath has been started
    } deriving (Eq, Show)

-- | State monad for incrementally building SVG path strings.
--   Each path command updates the cursor position and appends to the path data.
type PathBuilder = State PathState

-- https://www.w3.org/TR/SVG/paths.html#PathData

-- | MoveTo commands - start a new subpath
data MoveTo = M Point       -- ^ Absolute moveto
            | LittleM Point -- ^ Relative moveto
            deriving (Eq, Show)

-- | LineTo commands - draw straight lines from the current point
data LineTo = L Point        -- ^ Absolute lineto
            | LittleL Point  -- ^ Relative lineto
            | H Double       -- ^ Absolute horizontal lineto
            | LittleH Double -- ^ Relative horizontal lineto
            | V Double       -- ^ Absolute vertical lineto
            | LittleV Double -- ^ Relative vertical lineto
            deriving (Eq, Show)

-- | ClosePath commands - close the current subpath
data ClosePath = Z       -- ^ Close path
               | LittleZ -- ^ Close path (relative, same effect)
               deriving (Eq, Show)

-- | ArcTo commands - elliptical arc curves
--   Parameters: rx ry x-rotation large-arc-flag sweep-flag endpoint
data ArcTo = A Double Double Double Bool Bool Point       -- ^ Absolute arc
           | LittleA Double Double Double Bool Bool Point -- ^ Relative arc
           deriving (Eq, Show)

-- | Empty initial path state
emptyPathState :: PathState
emptyPathState = PathState (0, 0) "" False

-- | Run a path builder and extract the SVG path data string
buildPath :: PathBuilder a -> PathStr
buildPath builder = psPath (execState builder emptyPathState)

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

append :: Text -> PathBuilder ()
append t = modify' (\s -> s { psPath = psPath s <> t })

setCursor :: Point -> PathBuilder ()
setCursor p = modify' (\s -> s { psCursor = p, psHasPoint = True })

------------------------------------------------------------------------
-- Path operations
------------------------------------------------------------------------

-- | Execute a MoveTo command
moveTo :: MoveTo -> PathBuilder ()
moveTo (M (x, y)) = do
    append $ "M" <> tshow x <> "," <> tshow y
    setCursor (x, y)
moveTo (LittleM (dx, dy)) = do
    (cx, cy) <- gets psCursor
    append $ "m" <> tshow dx <> "," <> tshow dy
    setCursor (cx + dx, cy + dy)

-- | Execute a LineTo command
lineTo :: LineTo -> PathBuilder ()
lineTo (L (x, y)) = do
    append $ "L" <> tshow x <> "," <> tshow y
    setCursor (x, y)
lineTo (LittleL (dx, dy)) = do
    (cx, cy) <- gets psCursor
    append $ "l" <> tshow dx <> "," <> tshow dy
    setCursor (cx + dx, cy + dy)
lineTo (H x) = do
    (_, cy) <- gets psCursor
    append $ "H" <> tshow x
    setCursor (x, cy)
lineTo (LittleH dx) = do
    (cx, cy) <- gets psCursor
    append $ "h" <> tshow dx
    setCursor (cx + dx, cy)
lineTo (V y) = do
    (cx, _) <- gets psCursor
    append $ "V" <> tshow y
    setCursor (cx, y)
lineTo (LittleV dy) = do
    (cx, cy) <- gets psCursor
    append $ "v" <> tshow dy
    setCursor (cx, cy + dy)

-- | Execute a ClosePath command
closePath :: ClosePath -> PathBuilder ()
closePath Z       = append "Z"
closePath LittleZ = append "z"

-- | Execute an ArcTo command
arcTo :: ArcTo -> PathBuilder ()
arcTo (A rx ry xrot largeArc sweep (x, y)) = do
    append $ "A" <> tshow rx <> "," <> tshow ry
          <> " " <> tshow xrot
          <> " " <> (if largeArc then "1" else "0")
          <> "," <> (if sweep then "1" else "0")
          <> " " <> tshow x <> "," <> tshow y
    setCursor (x, y)
arcTo (LittleA rx ry xrot largeArc sweep (dx, dy)) = do
    (cx, cy) <- gets psCursor
    append $ "a" <> tshow rx <> "," <> tshow ry
          <> " " <> tshow xrot
          <> " " <> (if largeArc then "1" else "0")
          <> "," <> (if sweep then "1" else "0")
          <> " " <> tshow dx <> "," <> tshow dy
    setCursor (cx + dx, cy + dy)

-- | Canvas-style arc command (as used internally by d3).
--   Converts center/radius/angle parameters to SVG arc path commands.
--   Mirrors the semantics of the HTML Canvas arc() method and d3-path's arc().
--
--   @canvasArc cx cy r startAngle endAngle counterclockwise@
canvasArc :: Double -> Double -> Double -> Double -> Double -> Bool -> PathBuilder ()
canvasArc cx cy r a0 a1 ccw = do
    let dx0   = r * cos a0
        dy0   = r * sin a0
        x0    = cx + dx0
        y0    = cy + dy0
        sweep = not ccw
        da0   = if ccw then a0 - a1 else a1 - a0
        tau   = 2 * pi
        eps   = 1e-6
        -- Normalize delta angle to [0, tau)
        da = if da0 < 0
             then da0 - tau * fromIntegral (floor (da0 / tau) :: Int)
             else da0

    hasPoint <- gets psHasPoint
    if hasPoint
        then lineTo (L (x0, y0))
        else moveTo (M (x0, y0))

    when (r > eps) $
        if da > tau - eps
            then do
                -- Full circle: draw as two arcs via the antipodal point
                let mx = cx - dx0
                    my = cy - dy0
                arcTo $ A r r 0 True sweep (mx, my)
                arcTo $ A r r 0 True sweep (x0, y0)
            else when (da > eps) $ do
                let x1 = cx + r * cos a1
                    y1 = cy + r * sin a1
                    largeArc = da >= pi
                arcTo $ A r r 0 largeArc sweep (x1, y1)
