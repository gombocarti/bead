{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.Login (
    login
  , loginSubmit
  , changeLanguage
  ) where

-- Bead imports

import Bead.Controller.ServiceContext hiding (serviceContext)
import Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.UserStories as S
import Bead.View.Snap.Application
import Bead.View.Snap.Dictionary
import Bead.View.Snap.Session
import Bead.View.Snap.RouteOf
import Bead.View.Snap.HandlerUtils
import Bead.View.Snap.Pagelets
import Bead.View.Snap.ErrorPage (
    errorPageWithTitle
  , errorPage
  , errorPageWithTitleTrans
  )
import Bead.View.Snap.I18N
import Bead.View.Snap.Translation

import Bead.View.Snap.Content hiding (BlazeTemplate, template)
import Bead.View.Snap.Content.All

-- Haskell imports

import Prelude as P
import Data.Either (either)
import Data.String
import Data.ByteString.Char8 hiding (index, putStrLn)
import qualified Data.Text as T
import Control.Monad (join)

-- Snap and Blaze imports

import Snap hiding (get)
import Snap.Blaze (blaze)
import Snap.Snaplet.Auth as A
import Snap.Snaplet.Session

-- import Control.Monad (mapM_)

import Text.Blaze (textTag)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes hiding (title, rows, accept, method)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Bead.View.Snap.I18N (IHtml)

-- * Login and Logout handlers

login :: Maybe AuthFailure -> Handler App b ()
login authError = do
  languages <- withTop dictionaryContext dcGetDictionaryInfos
  renderPublicPage $ loginPage authError languages

loginSubmit :: Handler App b ()
loginSubmit = withTop auth $ handleError $ runErrorT $ do
  user <- getParameter loginUsernamePrm
  pwd  <- getParameter loginPasswordPrm
  loggedIn <- lift $ loginByUsername
    (usernameCata T.pack user)
    (ClearText . pack $ pwd)
    False
  case loggedIn of
    Left failure -> lift $ login $ visibleFailure $ failure
    Right authUser -> lift $ do
      context <- withTop serviceContext getServiceContext
      token   <- sessionToken
      let unameFromAuth = usernameFromAuthUser authUser
          mpasswFromAuth = passwordFromAuthUser authUser
      case mpasswFromAuth of
        Nothing -> do logMessage ERROR "No password was given"
                      A.logout
        Just passwFromAuth -> do
          result <- liftIO $ S.runUserStory context UserNotLoggedIn $ do
            S.login unameFromAuth token
            S.currentUser
          case result of
            Left err -> do
              logMessage ERROR $ "Error happened processing user story: " ++ show err
              -- Service context authentication
              liftIO $ (userContainer context) `userLogsOut` (userToken (unameFromAuth, token))
              A.logout
              withTop sessionManager $ commitSession
              errorPageWithTitleTrans
                (Msg_Login_PageTitle "Bejelentkezés")
                (Msg_Login_InternalError "Belső hiba történt, jelezd az üzemeltetőknek!")
            Right (user,userState) -> do
              initSessionValues (page userState) unameFromAuth (u_language user)
              withTop sessionManager $ commitSession
              redirect "/"
  return ()
  where
    handleError m =
      m >>= (either (login . Just . AuthError . contentHandlerErrorMsg) (const $ return ()))

    initSessionValues :: P.Page -> Username -> Language -> Handler App b ()
    initSessionValues page username language = do
      withTop sessionManager $ do
        setSessionVersion
        setLanguageInSession language
        setUsernameInSession username
        setActPageInSession  page
      withTop serviceContext $ do
        logMessage DEBUG $ "Username is set in session to: " ++ show username
        logMessage DEBUG $ "User's actual page is set in session to: " ++ show page

-- * Blaze --

userForm :: String -> IHtml
userForm act = do
  msg <- getI18N
  return $ postForm act $ do
    table (formId loginForm) (formId loginForm) $ do
      return ()
      tableLine (msg $ Msg_Login_Neptun "NEPTUN:") (textInput (fieldName loginUsername) 20 Nothing ! A.required "")
      tableLine (msg $ Msg_Login_Password "Jelszó:") (passwordInput (fieldName loginPassword) 20 Nothing ! A.required "")
    submitButton (fieldName loginSubmitBtn) (msg $ Msg_Login_Submit "Bejelentkezés")

loginPage :: Maybe AuthFailure -> DictionaryInfos -> IHtml
loginPage err langInfos = withTitleAndHead (Msg_Login_Title "Bejelentkezés") content
  where
    content = do
      msg <- getI18N
      return $ do
        i18n msg $ userForm "/login"
        maybe (return ())
              ((H.p ! A.style "font-size: smaller") . fromString . show)
              err
        H.p $ do
          H.a ! A.href "/reg_request" $ fromString $ msg $ Msg_Login_Registration "Regisztráció"
          H.br
          H.a ! A.href "/reset_pwd" $ fromString $ msg $ Msg_Login_Forgotten_Password "Elfelejtett jelszó"
        when ((P.length langInfos) > 1) $ do
          H.p $ do
            forM_ langInfos $ \(language,info) -> do
              -- TODO: I18N put the icon
              H.a ! A.href (queryString changeLanguagePath [requestParam language])
                $ (dictionaryInfoCata (\_icon -> fromString) info)
              H.br

-- TODO: I18N
-- Keeps only the authentication failures which are
-- visible for the user
visibleFailure :: AuthFailure -> Maybe AuthFailure
visibleFailure (AuthError e)     = Just (AuthError e)
visibleFailure IncorrectPassword = Just (AuthError "Hibás jelszó!")
visibleFailure UserNotFound      = Just (AuthError "A felhasználó nem található!")
visibleFailure _ = Nothing

-- * Change language in the session

changeLanguage :: Handler App b ()
changeLanguage = method GET setLanguage <|> method POST (redirect "/") where
  setLanguage = withTop sessionManager $ do
    elang <- getParameterOrError changeLanguagePrm
    either
      (liftIO . putStrLn)
      (\l -> do setLanguageInSession l
                withTop sessionManager commitSession)
      elang -- TODO: Log the error message
    redirect "/"
