
module Models.User where

import           Control.Monad
import           Control.Monad.State
import           Snap.Snaplet.Auth
import           Snap.Snaplet
import           Control.Monad.CatchIO (throw)
import qualified Data.Text as T

import           Models.Exception
import           Models.Utils

data LoginUser = LoginUser
    { loginName :: T.Text
    , password  :: T.Text
    , repeatPassword :: T.Text
    } deriving (Show)

------------------------------------------------------------------------------

-- | Create user without activation appoach thus login automatically.
--   Maybe use userLockedOutUntil when like to use mail activation.
--
createNewUser :: LoginUser -> Handler b (AuthManager b) AuthUser
createNewUser usr = do
    mp <- gets minPasswdLen
    when (passLength usr < mp) (throw $ PasswordTooShort mp)
    exists <- usernameExists (loginName usr)
    when exists (throw UserAlreadyExists)
    authUsr <- createUser (loginName usr) (password' usr)
    forceLogin authUsr >>= either throwUE return
  where passLength    = T.length . password
        password'     = textToBs . password

------------------------------------------------------------------------------

loginUser :: LoginUser -> Handler b (AuthManager b) AuthUser
loginUser u = do
              res <- loginByUsername (username' u) (password' u) True
              either throwUE return res
              where username' = textToBs . loginName
                    password' = ClearText . textToBs . password


------------------------------------------------------------------------------
