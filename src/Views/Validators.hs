{-# LANGUAGE OverloadedStrings #-}

module Views.Validators where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Digestive

import Models.Utils

-- | Mandatroy field validator.
-- 
requiredValidator :: T.Text -> Bool
requiredValidator = not . T.null . T.strip

-- | Check for required field with error message @msg@
-- 
checkForRequired :: Monad m => Text -> Form Text m Text -> Form Text m Text
checkForRequired msg = check msg requiredValidator

-- | Check for min length reqirued.
-- 
checkForMinLength :: Monad m => Int -> Form Text m Text -> Form Text m Text
checkForMinLength l = check ("Content is simple. min length " `T.append` sToText l ) minLength
                      where minLength = (>= l) . T.length  
