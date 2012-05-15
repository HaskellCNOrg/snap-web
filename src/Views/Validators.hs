
module Views.Validators where

import qualified Data.Text as T

requiredValidator :: T.Text -> Bool
requiredValidator = not . T.null . T.strip
