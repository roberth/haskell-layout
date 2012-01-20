module Text.Layout ( module Text.Layout.Objects
                   , module Text.Layout.DisplayText
                   ) where
import Text.Layout.Objects
import Text.Layout.DisplayText

{---- begin TODO move to modules ----}

import Data.Convertible

newtype DisplayHtml = DisplayHtml String
newtype DisplayLatex = DisplayLatex String

instance Convertible DisplayHtml DisplayHtml where
  safeConvert = Right

instance Convertible DisplayLatex DisplayLatex where
  safeConvert = Right

{---- end TODO move to modules ----}