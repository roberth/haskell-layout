module Text.Layout.DisplayText ( DisplayText(..)
                               ) where
import Control.Arrow
import Data.Convertible
import Data.List
import Data.List.HIUtils
import Data.Maybe
import Data.Ord
import Data.String.HIUtils
import Text.Layout.Objects

newtype DisplayText = DisplayText { fromDisplayText :: String }

-- This Show instance is for ghci use -- there is no Read
instance Show DisplayText where
  show = fromDisplayText

instance Convertible DisplayText DisplayText where
  safeConvert = Right

instance (Convertible a DisplayText,
          Convertible b DisplayText,
          Convertible c DisplayText) =>
  Convertible (Table a b c) DisplayText where
    safeConvert (Table caption (x, y) sparseRepr) = Right $ DisplayText s
      where s = asciiTable caption (x, y) sparseRepr


-- for x, x' of types a, b, or c, we assume show x == show x' <==> x == x'
asciiTable :: --(Show a, Eq a, Show b, Eq b, Show c, Eq c)
  ( Convertible a DisplayText
  , Convertible b DisplayText
  , Convertible c DisplayText ) => 
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

        rowhdr = bstr ++ "\x2193"
        rhlen = maximum $ map length (rowhdr : rows)
        
        renderrow (rn, rv) = padl rhlen rn ++ " " ++
                              (concat . intersperse " " . map
                                (renderval rv)) collengths
        renderval rv (c,l) = padl l $ fromMaybe "" $ lookup c rv
        padval (col,v) = maybe "" (flip padl v) $ lookup col collengths
        collengths = [ (colname, maximum $ map length xs) |
                       (colname, colvals) <- colAL, let xs = (colname:colvals) ]
        thead = thead1 ++ "\n" ++ thead2
        thead1 = padl rhlen "" ++ " \x2192" ++ astr
        thead2 = padl rhlen rowhdr ++ " " ++ concat (intersperse " " (map (uncurry (flip padl)) collengths))
        tbody = concat . intersperse "\n" . map renderrow $ rowsAL
