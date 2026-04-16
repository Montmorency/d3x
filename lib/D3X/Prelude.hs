{-# LANGUAGE ImplicitPrelude #-}
{-
  Module : D3X.Prelude
  Description : Exposes some useful type classes and combinators for data visualization.
  Copyright   : (c) Comhlan Ltd. 2026
-}
module D3X.Prelude
    ( Text
    , tshow
    , cs
    , forEach
    ) where

import Data.Text (Text, pack)
import Data.String.Conversions (cs)
import IHP.HSX.Markup (Html)

tshow :: Show a => a -> Text
tshow = pack . show

-- | Map each element to Html and concatenate via the markup Monoid.
--   Drop-in replacement for IHP's @forEach@ that avoids the mono-traversable /
--   full-IHP dependency, so @d3x@ can stay a small library suitable for Hackage.
--   Use inside HSX splices: @{forEach xs renderRow}@.
forEach :: Foldable t => t a -> (a -> Html) -> Html
forEach xs f = foldMap f xs
