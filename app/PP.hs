module PP (module P, module PP) where

import Data.Text
import Data.Text qualified as Text
import Text.PrettyPrint hiding ((<>))
import Text.PrettyPrint qualified as P

infixl 6 <.>
(<.>) :: Doc -> Doc -> Doc
(<.>) = (P.<>)

class PP a where
  pp :: a -> Doc

-- | Shows text without quotes
instance PP Text where
  pp = text . Text.unpack