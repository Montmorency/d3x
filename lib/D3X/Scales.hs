{-# LANGUAGE ImplicitPrelude #-}
module D3X.Scales where

import D3X.Prelude
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Clock (UTCTime(..), diffUTCTime)

-- | CSS coordinate value - absolute length or percentage
--   https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/length
data Coordinate = Length Double
                | Percentage Double
                deriving (Eq, Show)

-- | Domain wraps the (min, max) bounds of the input space
newtype Domain a = Domain (a, a) deriving (Eq, Show)

-- | Range wraps the (min, max) bounds of the output coordinate space
newtype Range = Range (Coordinate, Coordinate) deriving (Eq, Show)

-- | Extract the numeric value from a Coordinate
coordVal :: Coordinate -> Double
coordVal (Length n)     = n
coordVal (Percentage n) = n

-- | Display a Coordinate as Text for SVG attributes
showCoord :: Coordinate -> Text
showCoord (Length n)     = tshow n
showCoord (Percentage n) = tshow n <> "%"

-- | Interpolate within a Range given a normalised parameter t ∈ [0,1].
--   Preserves the Coordinate constructor (Length or Percentage).
interpolateRange :: Double -> Range -> Coordinate
interpolateRange t (Range (Length rMin, Length rMax))         = Length     (rMin + t * (rMax - rMin))
interpolateRange t (Range (Percentage rMin, Percentage rMax)) = Percentage (rMin + t * (rMax - rMin))
interpolateRange _ _ = Length 0

-- | LinearScale maps values from a Domain to a Range via linear interpolation.
--   Define instances for each input type you need to scale.
--
-- === d3.js equivalent
-- @
-- const x = d3.scaleLinear([10, 130], [0, 960]);
-- x(20);  // 80
-- x(50);  // 320
-- @
--
-- === Haskell
-- @
-- let x = linearScale (Domain (10, 130)) (Range (Length 0, Length 960))
-- x 20  -- Length 80.0
-- x 50  -- Length 320.0
-- @
class LinearScale a where
  linearScale :: Domain a -> Range -> (a -> Coordinate)

instance LinearScale Double where
  linearScale (Domain (dMin, dMax)) range x
    | abs (dMax - dMin) < 1e-10 = interpolateRange 0 range
    | otherwise = interpolateRange ((x - dMin) / (dMax - dMin)) range

instance LinearScale Int where
  linearScale (Domain (dMin, dMax)) range x
    | dMax == dMin = interpolateRange 0 range
    | otherwise =
        let t = fromIntegral (x - dMin) / fromIntegral (dMax - dMin)
        in interpolateRange t range

instance LinearScale Day where
  linearScale (Domain (dMin, dMax)) range x
    | diffDays dMax dMin == 0 = interpolateRange 0 range
    | otherwise =
        let t = fromIntegral (diffDays x dMin) / fromIntegral (diffDays dMax dMin)
        in interpolateRange t range

instance LinearScale UTCTime where
  linearScale (Domain (dMin, dMax)) range x
    | diffUTCTime dMax dMin == 0 = interpolateRange 0 range
    | otherwise =
        let t = realToFrac (diffUTCTime x dMin) / realToFrac (diffUTCTime dMax dMin) :: Double
        in interpolateRange t range
