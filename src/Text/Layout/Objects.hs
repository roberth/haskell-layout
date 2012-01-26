module Text.Layout.Objects where
import Control.Arrow

data Table a b c = Table { tableCaption :: String
                         , tableAxes    :: (String, String) 
                         , tableValues  :: [((a,b), c)]
                         }

instance Functor (Table a b) where
  fmap f x = x { tableValues = map (second f) $ tableValues x }
