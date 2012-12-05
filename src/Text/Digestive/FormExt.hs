{-# LANGUAGE OverloadedStrings #-}

--
-- Extension to Text.Digestive.Form.
-- Basically more utils.
--

module Text.Digestive.FormExt where

import           Data.Maybe     (isJust, fromMaybe)
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Text.Digestive


---------------------------------------------------- Validator

-- | MAYBE: more accurate email addr. validator.
--
emailValidator :: T.Text -> Bool
emailValidator = isJust . T.find (== '@')


-- | Mandatroy field validator.
--
requiredValidator :: T.Text -> Bool
requiredValidator = not . T.null . T.strip


maxListValidator :: Int                  -- ^ Max
                    -> (Text -> [Text])  -- ^ Convert to list
                    -> Text              -- ^ Input text
                    -> Bool
maxListValidator n f = (<= n) . length . f


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
checkMinLength l = checkMinLengthWith l "Content"
                      
checkMinLengthWith :: Monad m
                      => Int
                      -> Text
                      -> Form Text m Text -> Form Text m Text
checkMinLengthWith l msg = check (msg `T.append` " is too simple. min length " `T.append` intToText) minLength
                      where minLength = (>= l) . T.length
                            intToText = T.pack (show l)


-- | Check for max length reqirued.
--
checkMaxLength :: Monad m => Int -> Form Text m Text -> Form Text m Text
checkMaxLength l = checkMaxLengthWith l "Content"

checkMaxLengthWith :: Monad m
                      => Int
                      -> Text
                      -> Form Text m Text -> Form Text m Text
checkMaxLengthWith l msg = check (msg `T.append` " exceeds max length " `T.append` intToText) maxLength
                      where maxLength = (<= l) . T.length
                            intToText = T.pack (show l)
