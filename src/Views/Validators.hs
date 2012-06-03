{-# LANGUAGE OverloadedStrings #-}

module Views.Validators where

import qualified Data.Text as T
import Data.Maybe (isJust)
import Data.Text (Text)
import Text.Digestive

import Models.Utils

---------------------------------------------------- Validator

-- | Maybe: more accurate email addr. validator.
--
emailValidator :: T.Text -> Bool
emailValidator = isJust . T.find (== '@')


-- | Mandatroy field validator.
-- 
requiredValidator :: T.Text -> Bool
requiredValidator = not . T.null . T.strip


---------------------------------------------------- Validate Action


-- | Check for required field with error message @msg@
-- 
checkRequired :: Monad m => Text -> Form Text m Text -> Form Text m Text
checkRequired msg = check msg requiredValidator


-- | Check for required field with error message @msg@
-- 
checkValidEmail :: Monad m => Form Text m Text -> Form Text m Text
checkValidEmail = check "Please input valid email address." emailValidator


-- | Check for min length reqirued.
-- 
checkMinLength :: Monad m => Int -> Form Text m Text -> Form Text m Text
checkMinLength l = check ("Content is simple. min length " `T.append` sToText l ) minLength
                      where minLength = (>= l) . T.length  
