module Text.Layout ( module Text.Layout.Class
                   , module Text.Layout.Objects
                   , module Text.Layout.DisplayText
                   , module Text.Layout.DisplayLatex
                   ) where
import Text.Layout.Class
import Text.Layout.Objects
import Text.Layout.DisplayText
import Text.Layout.DisplayLatex

{---- begin TODO move to modules ----}

import Data.Convertible

newtype DisplayHtml = DisplayHtml String

instance Convertible DDisplayHtml DisplayHtml where
  safeConvert = Right

{---- end TODO move to modules ----}