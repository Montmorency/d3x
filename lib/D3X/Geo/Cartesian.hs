{-# LANGUAGE ImplicitPrelude #-}
-- | 3D Cartesian utilities used by 'D3X.Geo.Rotation', 'D3X.Geo.Resample',
--   and 'D3X.Geo.Clip' (the spherical-cap clipper).
--
--   Mirrors @d3-geo/src/cartesian.js@.
module D3X.Geo.Cartesian
    ( Cart
    , sphericalToCartesian
    , cartesianToSpherical
    , cartesianDot
    , cartesianCross
    , cartesianAdd
    , cartesianScale
    , cartesianNorm
    , cartesianNormalize
    ) where

-- | A unit-sphere 3D Cartesian point @(x, y, z)@.
type Cart = (Double, Double, Double)

-- | @(lambda, phi)@ in radians → @(x, y, z)@ on the unit sphere.
sphericalToCartesian :: Double -> Double -> Cart
sphericalToCartesian lambda phi =
    let cphi = cos phi
    in (cphi * cos lambda, cphi * sin lambda, sin phi)

-- | @(x, y, z)@ → @(lambda, phi)@ in radians.
cartesianToSpherical :: Cart -> (Double, Double)
cartesianToSpherical (x, y, z) = (atan2 y x, asin z)

cartesianDot :: Cart -> Cart -> Double
cartesianDot (a, b, c) (d, e, f) = a*d + b*e + c*f

cartesianCross :: Cart -> Cart -> Cart
cartesianCross (a, b, c) (d, e, f) = (b*f - c*e, c*d - a*f, a*e - b*d)

cartesianAdd :: Cart -> Cart -> Cart
cartesianAdd (a, b, c) (d, e, f) = (a+d, b+e, c+f)

cartesianScale :: Double -> Cart -> Cart
cartesianScale k (a, b, c) = (k*a, k*b, k*c)

cartesianNorm :: Cart -> Double
cartesianNorm (a, b, c) = sqrt (a*a + b*b + c*c)

-- | Normalise to the unit sphere. Returns the input unchanged if its
--   norm is zero (degenerate, should never happen in practice).
cartesianNormalize :: Cart -> Cart
cartesianNormalize v@(a, b, c) =
    let n = cartesianNorm v
    in if n == 0 then v else (a/n, b/n, c/n)
