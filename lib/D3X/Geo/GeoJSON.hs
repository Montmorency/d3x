{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | GeoJSON algebra (RFC 7946) plus aeson decoders.
--
--   Properties on @Feature@ are kept opaque as @Aeson.Value@ so callers
--   decode their own typed records via @Data.Aeson.parseEither parseJSON@.
module D3X.Geo.GeoJSON
    ( -- * Coordinates
      Position(..)
      -- * Geometry
    , Geometry(..)
      -- * Features
    , Feature(..)
    , FeatureCollection(..)
      -- * Object
    , GeoObject(..)
      -- * Helpers
    , decodeFeatureCollection
    , decodeFeatureProperties
    ) where

import D3X.Prelude (Text)
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?), (.!=), withObject, withArray, eitherDecodeStrict)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonT
import Data.ByteString (ByteString)
import qualified Data.Vector as Vector

-- | A 2D coordinate. We drop optional altitude/M to keep the inner loop tight;
--   3D GeoJSON loaders can ignore the third element.
data Position = Position !Double !Double
  deriving (Eq, Show)

instance FromJSON Position where
    parseJSON = withArray "Position" $ \arr ->
        case Vector.toList arr of
            (x:y:_) -> Position <$> parseJSON x <*> parseJSON y
            _ -> fail "Position must have at least 2 coordinates"

data Geometry
    = Point              !Position
    | MultiPoint         ![Position]
    | LineString         ![Position]
    | MultiLineString    ![[Position]]
    | Polygon            ![[Position]]            -- ^ outer ring + holes
    | MultiPolygon       ![[[Position]]]
    | GeometryCollection ![Geometry]
    | Sphere
    deriving (Eq, Show)

instance FromJSON Geometry where
    parseJSON = withObject "Geometry" $ \o -> do
        ty <- o .: "type" :: AesonT.Parser Text
        case ty of
            "Point"              -> Point              <$> o .: "coordinates"
            "MultiPoint"         -> MultiPoint         <$> o .: "coordinates"
            "LineString"         -> LineString         <$> o .: "coordinates"
            "MultiLineString"    -> MultiLineString    <$> o .: "coordinates"
            "Polygon"            -> Polygon            <$> o .: "coordinates"
            "MultiPolygon"       -> MultiPolygon       <$> o .: "coordinates"
            "GeometryCollection" -> GeometryCollection <$> o .: "geometries"
            "Sphere"             -> pure Sphere
            other                -> fail ("Unknown geometry type: " <> show other)

data Feature = Feature
    { featGeometry   :: !(Maybe Geometry)
    , featProperties :: !Value             -- ^ opaque; decode in caller
    , featId         :: !(Maybe Value)
    } deriving (Eq, Show)

instance FromJSON Feature where
    parseJSON = withObject "Feature" $ \o -> do
        Feature
            <$> o .:? "geometry"  .!= Nothing
            <*> o .:? "properties" .!= Null
            <*> o .:? "id"

newtype FeatureCollection = FeatureCollection
    { fcFeatures :: [Feature] }
    deriving (Eq, Show)

instance FromJSON FeatureCollection where
    parseJSON = withObject "FeatureCollection" $ \o ->
        FeatureCollection <$> o .: "features"

-- | The single input type accepted by 'D3X.Geo.Path.geoPath'.
data GeoObject
    = GoFeature            !Feature
    | GoFeatureCollection  !FeatureCollection
    | GoGeometry           !Geometry
    | GoGeometryCollection ![Geometry]
    deriving (Eq, Show)

-- | Convenience: decode a strict ByteString into a 'FeatureCollection'.
decodeFeatureCollection :: ByteString -> Either String FeatureCollection
decodeFeatureCollection = eitherDecodeStrict

-- | Decode the @properties@ payload of a 'Feature' into a user-defined record.
decodeFeatureProperties :: FromJSON p => Feature -> Either String p
decodeFeatureProperties = AesonT.parseEither parseJSON . featProperties
