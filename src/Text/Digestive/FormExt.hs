{-# LANGUAGE OverloadedStrings #-}

-- 
-- Extension to Text.Digestive.Form.
-- Basically more utils.
--

module Text.Digestive.FormExt where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Digestive


---------------------------------------------------- Validator

-- | MAYBE: more accurate email addr. validator.
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
checkMinLength l = check ("Content is too simple. min length " `T.append` intToText) minLength
                      where minLength = (>= l) . T.length  
                            intToText = T.pack (show l)


-- | Check for max length reqirued.
-- 
checkMaxLength :: Monad m => Int -> Form Text m Text -> Form Text m Text
checkMaxLength l = check ("Content exceeds max length " `T.append` intToText) maxLength
                      where maxLength = (<= l) . T.length  
                            intToText = T.pack (show l)
