
module Views.Validators where

import qualified Data.Text as T

-- | Mandatroy field validator.
-- 
requiredValidator :: T.Text -> Bool
requiredValidator = not . T.null . T.strip

