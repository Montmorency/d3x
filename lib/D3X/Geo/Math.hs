{-# LANGUAGE ImplicitPrelude #-}
-- | Constants and trivial conversions used by D3X.Geo.* modules.
--   Mirrors d3-geo's @src/math.js@.
module D3X.Geo.Math
    ( -- * Constants
      tau
    , halfPi
    , epsilon
    , epsilon2
      -- * Conversions
    , radians
    , degrees
    ) where

tau, halfPi, epsilon, epsilon2 :: Double
tau     = 2 * pi
halfPi  = pi / 2
epsilon = 1e-6
epsilon2 = epsilon * epsilon

-- | Degrees to radians.
radians :: Double -> Double
radians d = d * pi / 180

-- | Radians to degrees.
degrees :: Double -> Double
degrees r = r * 180 / pi
