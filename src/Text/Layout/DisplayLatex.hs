module Text.Layout.DisplayLatex where
import Data.Convertible

newtype DisplayLatex = DisplayLatex { fromDisplayLatex :: String }

-- | Shortcut for convert
dltx :: (Convertible a DisplayLatex) => a -> DisplayLatex
dltx = convert

-- This Show instance is for use with ghci -- there is no Read
instance Show DisplayLatex where
  show = fromDisplayLatex

instance Convertible DisplayLatex DisplayLatex where
  safeConvert = Right
