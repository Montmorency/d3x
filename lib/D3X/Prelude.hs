{-
  Module : D3X.Prelude
  Description : Exposes some useful type classes and combinators for data visualization.
  Copyright   : (c) Comhlan Ltd. 2026 
-}
module D3X.Prelude
    ( Text
    , tshow
    , cs
    ) where

import Data.Text (Text, pack)
import Data.String.Conversions (cs)

tshow :: Show a => a -> Text
tshow = pack . show
