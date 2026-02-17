module D3X.Prelude
    ( Text
    , tshow
    , cs
    ) where

import Data.Text (Text, pack)
import Data.String.Conversions (cs)

tshow :: Show a => a -> Text
tshow = pack . show
