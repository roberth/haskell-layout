module Text.Layout.Objects where

data Table a b c = Table String (String, String) [((a,b), c)]
