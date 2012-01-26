module Text.Layout.Class where
import Data.List
import Data.Monoid

class (Monoid format) => IsFormat format where
  formatVerbatim :: String -> format
  fromShow :: (Show a) => a -> format
  fromShow = formatVerbatim . show

class (IsFormat format) => Layout a format where
  format :: a -> format
  formatList :: [a] -> format -- required to implement exception for String
  formatList as = formatVerbatim "[" `mappend`
                  (mconcat $ intersperse (formatVerbatim ", ") $ map format as) `mappend` 
                  formatVerbatim "]"
