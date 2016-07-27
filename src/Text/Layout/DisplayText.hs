{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Text.Layout.DisplayText ( DisplayText(..)
                               , dt
                               ) where
import Control.Arrow
import Data.Convertible
import Data.List
import Data.List.HIUtils
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.String.HIUtils
import Text.Layout.Objects
import Text.Layout.Class

newtype DisplayText = DisplayText { fromDisplayText :: String }

-- This Show instance is for use with ghci -- there is no Read
instance Show DisplayText where
  show = fromDisplayText
  
instance Monoid DisplayText where
  mempty = DisplayText ""
  a `mappend` b = DisplayText $ 
                  (fromDisplayText a) `mappend` (fromDisplayText b)
  mconcat = DisplayText . mconcat . map fromDisplayText

instance IsFormat DisplayText where
  formatVerbatim = DisplayText

instance Layout DisplayText DisplayText where
  format = id

instance Layout Char DisplayText where
  format = fromShow
  formatList = fromShow
  
instance (Layout a DisplayText) => Layout [a] DisplayText where
  format = formatList

instance (Layout a DisplayText) => Convertible a DisplayText where
  safeConvert = Right . format

--instance Convertible DisplayText DisplayText where
--  safeConvert = Right

instance Layout () DisplayText where format = fromShow
instance Layout Integer DisplayText where format = fromShow
instance Layout Int DisplayText where format = fromShow
instance Layout Float DisplayText where format = fromShow
instance Layout Double DisplayText where format = fromShow
instance (Show (Ratio a)) => Layout (Ratio a) DisplayText where format = fromShow
instance (Show a) => Layout (Maybe a) DisplayText where format = fromShow
instance (Show a, Show b) => Layout (Either a b) DisplayText where format = fromShow
instance (Show a, Show b) => Layout (a, b) DisplayText where format = fromShow
instance (Show a, Show b, Show c) => Layout (a, b, c) DisplayText where format = fromShow
instance (Show a, Show b, Show c, Show d) => Layout (a, b, c, d) DisplayText where format = fromShow
instance (Show a, Show b, Show c, Show d, Show e) => Layout (a, b, c, d, e) DisplayText where format = fromShow
instance (Show a, Show b, Show c, Show d, Show e, Show f) => Layout (a, b, c, d, e, f) DisplayText where format = fromShow
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Layout (a, b, c, d, e, f, g) DisplayText where format = fromShow
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Layout (a, b, c, d, e, f, g, h) DisplayText where format = fromShow
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Layout (a, b, c, d, e, f, g, h, i) DisplayText where format = fromShow

-- | Shortcut for @(Layout :: a -> DisplayText)@
dt :: (Layout a DisplayText) => a -> DisplayText
dt = format
 
instance (Layout a DisplayText,
          Layout b DisplayText,
          Layout c DisplayText) =>
  Layout (Table a b c) DisplayText where
    format (Table caption (x, y) sparseRepr) = formatVerbatim s
      where s = asciiTable caption (x, y) sparseRepr

-- for x, x' of types a, b, or c, we assume show x == show x' <==> x == x'
asciiTable :: --(Show a, Eq a, Show b, Eq b, Show c, Eq c)
  ( Layout a DisplayText
  , Layout b DisplayText
  , Layout c DisplayText ) =>
  String -> (String, String) -> [((a,b), c)] -> String
asciiTable title (astr, bstr) table' = "Table: " ++ title ++ "\n"
                                  ++ thead ++ "\n" ++ tbody
  where
        table = map ((renderdt *** renderdt) *** renderdt) table'
        renderdt :: (Convertible t DisplayText) => t -> String
        renderdt = fromDisplayText . convert
        
        colAL = sortBy (comparing fst) $ aggregateAL $ map (first fst) table
        rowsAL = sortBy (comparing fst) $ aggregateAL $ map (\((a,b), c) -> (b, (a, c))) table
        
        rows = map fst rowsAL
        cols = map fst colAL

        rowhdr = bstr ++ "\\/" -- "\x2193"
        rhlen = maximum $ map length (rowhdr : rows)
        
        renderrow (rn, rv) = padl rhlen rn ++ " " ++
                              (concat . intersperse " " . map
                                (renderval rv)) collengths
        renderval rv (c,l) = padl l $ fromMaybe "" $ lookup c rv
        padval (col,v) = maybe "" (flip padl v) $ lookup col collengths
        collengths = [ (colname, maximum $ map length xs) |
                       (colname, colvals) <- colAL, let xs = (colname:colvals) ]
        thead = thead1 ++ "\n" ++ thead2
        thead1 = padl rhlen "||" ++ " " ++ astr ++ " ==>" {-" \x2192"-}
        thead2 = padl rhlen rowhdr ++ " " ++ concat (intersperse " " (map (uncurry (flip padl)) collengths))
        tbody = concat . intersperse "\n" . map renderrow $ rowsAL
