{-# LANGUAGE ImplicitPrelude #-}
-- | Pre-projection Euler rotation in spherical coordinates (radians).
--
--   Mirrors @d3-geo/src/rotation.js@.
module D3X.Geo.Rotation
    ( rotateRadians
      -- * Pure point-level helpers (exported for testing)
    , rotateLambda
    , rotatePhiGamma
    ) where

import D3X.Geo.Math (tau)
import D3X.Geo.Stream (Stream, mapPoint)

-- | Rotate every emitted point by the given Euler angles
--   @(deltaLambda, deltaPhi, deltaGamma)@ (all in radians).
--   Falls back to the identity transformer when all three are zero.
rotateRadians :: (Double, Double, Double) -> Stream r -> Stream r
rotateRadians (dl, dp, dg)
    | dl /= 0 && (dp /= 0 || dg /= 0) = mapPoint composed
    | dl /= 0                         = mapPoint (rotateLambda dl)
    | dp /= 0 || dg /= 0              = mapPoint (rotatePhiGamma dp dg)
    | otherwise                       = id
  where
    composed lambda phi =
        let (l1, p1) = rotateLambda dl lambda phi
        in rotatePhiGamma dp dg l1 p1

-- | Longitude shift, wrapping back into @[-pi, pi]@.
rotateLambda :: Double -> Double -> Double -> (Double, Double)
rotateLambda dl lambda phi =
    let l   = lambda + dl
        l'  | l >  pi = l - tau
            | l < -pi = l + tau
            | otherwise = l
    in (l', phi)

-- | Combined latitude (phi) and roll (gamma) rotation, computed by
--   converting to 3D Cartesian, applying rotation matrices, and
--   converting back to spherical.
rotatePhiGamma :: Double -> Double -> Double -> Double -> (Double, Double)
rotatePhiGamma dp dg lambda phi =
    let cosDp = cos dp
        sinDp = sin dp
        cosDg = cos dg
        sinDg = sin dg
        cosPhi = cos phi
        x = cos lambda * cosPhi
        y = sin lambda * cosPhi
        z = sin phi
        k = z * cosDp + x * sinDp
    in ( atan2 (y * cosDg - k * sinDg) (x * cosDp - z * sinDp)
       , asin (k * cosDg + y * sinDg)
       )
