module Text.Digestive.Validations.Format
  ( Format(..)
  ) where

import Data.Text(Text)

class Format a where
  format ::  a -> Text
