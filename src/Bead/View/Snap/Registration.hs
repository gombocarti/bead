{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Snap.Registration (
    registration
  , createAdminUser
  ) where

-- Bead imports

import Bead.Controller.ServiceContext hiding (serviceContext)
import Bead.Controller.Logging as L

import qualified Bead.Controller.UserStories as S
import qualified Bead.Controller.Pages as P (Page(Login))
import Bead.View.Snap.Application
import Bead.View.Snap.Session
import Bead.View.Snap.HandlerUtils
import qualified Bead.Persistence.Persist as P (Persist(..), runPersist)

import Bead.View.Snap.Content hiding (BlazeTemplate, template, empty, method)

-- Haskell imports

import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.List as L

-- Snap and Blaze imports

import Snap hiding (get)
import Snap.Snaplet.Auth as A
import Snap.Snaplet.Auth.Backends.JsonFile (mkJsonAuthMgr)

import Text.Blaze (textTag)
import Text.Blaze.Html5 (Html, (!)) -- hiding (base, map, head, menu)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A hiding (title, rows, accept)

createAdminUser :: P.Persist -> FilePath -> String -> String -> IO ()
createAdminUser persist usersdb name password = do
  mgr <- mkJsonAuthMgr usersdb
  pwd <- encryptPassword . ClearText . fromString $ password
  let authUser = defAuthUser {
      userLogin    = fromString name
    , userPassword = Just pwd
    }
  save mgr authUser
  let usr = User {
      u_role = Admin
    , u_username = Username name
    , u_email = Email ""
    , u_name = ""
    }
  createdUser <- lookupByLogin mgr (T.pack name)
  case createdUser of
    Nothing -> error "No user was created"
    Just u' -> P.runPersist $ P.saveUser persist usr (passwordFromAuthUser u')
  return ()

-- * User registration handler

registration :: Handler App (AuthManager App) ()
registration = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = blaze $ newUser

    -- Registers a user as a Student, the administrator can grant
    -- better access to the user
    handleFormSubmit = do
      -- Register the user in the Snap auth module
      registerUser
        (fieldName loginUsername)
        (fieldName loginPassword)
      -- Register the user in the service context module
      uname      <- getParam (fieldName loginUsername)
      passwBS    <- getParam (fieldName loginPassword)
      email      <- getParam (fieldName regEmailAddress)
      fullname   <- getParam (fieldName regFullName)
      case (uname,email,fullname) of
        (Just u,Just e, Just f) -> do
          -- Create a user info for the service context and
          -- read the created user info from the Snap auth service context
          -- and run the registration user story in the service context
          -- with a freshly created (and encrypted) user password
          -- in the user context
          let usr = User {
                u_role = Student
              , u_username = asUsername u
              , u_email = Email . unpack $ e
              , u_name = unpack f
              }
          context <- withTop serviceContext $ getServiceContext
          createdUser <- withBackend $ \r -> liftIO $ lookupByLogin r (T.pack $ unpack u)
          case createdUser of
            Nothing -> withTop serviceContext . logMessage ERROR $
                         "User was not created at the first stage"
            Just u' -> do
              result  <- liftIO $ S.runUserStory context Registration
                           (S.createUser usr (passwordFromAuthUser u'))
              case result of
                Left err -> withTop serviceContext . logMessage ERROR . show $ err
                _        -> withTop serviceContext . logMessage INFO . show $
                              "Everything went fine. The user is created."

        _ -> withTop serviceContext . logMessage ERROR $
               "Username, email, or family name was not provided by the form"
      -- It does not matter what happens we redirect to the "/" page
      redirect "/"

-- * Blaze

newUser :: Html
newUser = dynamicTitleAndHead "Registration" content
  where
    content = do
      H.h1 $ "Register a new user"
      registrationForm "/new_user"
      linkToPageWithText P.Login "Go back to the login page"

registrationForm :: String -> Html
registrationForm postAction = do
  postForm postAction ! (A.id . formId $ regForm) $ do
    table (fieldName registrationTable) (fieldName registrationTable) $ do
      tableLine "Username:"      $ textInput (fieldName loginUsername)     20 Nothing ! A.required ""
      tableLine "Password:"      $ passwordInput (fieldName loginPassword) 20 Nothing ! A.required ""
      tableLine "Email address:" $ textInput (fieldName regEmailAddress)   20 Nothing ! A.required ""
      tableLine "Full name:"     $ textInput (fieldName regFullName)       20 Nothing ! A.required ""
    submitButton (fieldName regSubmitBtn) "Register"
