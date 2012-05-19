module Models.User where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.State
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Monad.Trans
import           Text.Templating.Heist
import           Control.Monad.CatchIO (try, throw,  Exception(..))
import qualified Data.Text as T

import           Controllers.Utils

data LoginUser = LoginUser
    { loginName :: T.Text
    , password  :: T.Text
    , repeatPassword :: T.Text
    } deriving (Show)

------------------------------------------------------------------------------

createNewUser :: LoginUser -> Handler b (AuthManager b) ()
createNewUser usr = do
    mp <- gets minPasswdLen
    -- when (passLength usr < mp) (throw $ BackendError $ "password length must longer than " ++ show mp)
    -- ^ Password min length validation
    exists <- usernameExists (loginName usr)
    when exists (throw $ BackendError "User Already Exists.")
    createUser (loginName usr) (password' usr)
    return ()
  where passLength    = T.length . password
        password'     = textToBs . password

------------------------------------------------------------------------------

loginUser :: LoginUser -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
loginUser u = loginByUsername (username' u) (password' u) True
              where username' = textToBs . loginName
                    password' = ClearText . textToBs . password