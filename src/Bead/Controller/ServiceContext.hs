{-# LANGUAGE TemplateHaskell #-}
module Bead.Controller.ServiceContext (
    UserState(..)
  , StatusMessage(..)
  , fullNameInState
  , statusMessage
  , userStateCata
  , userStateKindCata
  , userRole
  , getStatus
  , setStatus
  , clearStatus
  , setLanguage
  , getLanguage
  , setTimeZone
  , getTimeZone
  , usernameInState
  , userNotLoggedIn
  , ServiceContext(..)
  , serviceContext
  ) where

import qualified Control.Lens as Lens
import           Control.Lens ((^?))
import           Data.Semigroup (getFirst)
import           Data.Time (UTCTime)
import           Data.UUID (UUID)

import           Bead.Controller.Pages as Pages
import           Bead.Controller.Logging
import           Bead.Domain.Entities as Entities
import           Bead.View.Translation
import qualified Bead.Persistence.Persist as Persist

data UserState
  = UserNotLoggedIn {
      _language :: Language -- User's preferred language, set on log-in page
    }
  | Registration
  | TestAgent
  | UserLoggedIn {
      user :: Username -- Username
    , uid  :: Uid
    , name :: String   -- User's full name
    , _language :: Language -- User's preferred language
    , role :: Role     -- User's role
    , uuid :: UUID     -- Token for the active user session
    , _timeZone :: TimeZoneName -- Timezone of the user
    , _status :: Maybe (StatusMessage (Translation String)) -- The last status message
    } deriving Eq

Lens.makeLenses ''UserState

userStateCata
  userNotLoggedIn
  registration
  testAgent
  userLoggedIn
  s = case s of
    UserNotLoggedIn language -> userNotLoggedIn language
    Registration -> registration
    TestAgent -> testAgent
    UserLoggedIn u ui n l r uuid tz s -> userLoggedIn u ui n l r uuid tz s

userStateKindCata :: a -> a -> a -> a -> UserState -> a
userStateKindCata
  userNotLoggedIn
  registration
  testAgent
  userLoggedIn
  s = case s of
    UserNotLoggedIn _language -> userNotLoggedIn
    Registration -> registration
    TestAgent -> testAgent
    UserLoggedIn _u _ui _n _l _r _uuid _tz _s -> userLoggedIn

userNotLoggedIn :: Language -> UserState
userNotLoggedIn = UserNotLoggedIn

-- Converts the user state to a Role
userRole :: UserState -> Either OutsideRole Role
userRole = userStateCata
             (const (Left EmptyRole)) -- UserNotLoggedIn
             (Left RegRole)           -- Registration
             (Left TestAgentRole)     -- TestAgent
             (\_ _ _ _ role _ _ _ -> Right role) -- UserLoggedIn

-- Produces a new user state from the old one, setting
-- the status message to the given one
setStatus :: StatusMessage (Translation String)
          -> UserState
          -> UserState
setStatus msg = Lens.set status (Just msg)

-- Produces the status message of the UserState, otherwise Nothing
getStatus :: UserState -> Maybe (StatusMessage (Translation String))
getStatus = userStateCata
              (const Nothing)  -- UserNotLoggedIn
              Nothing          -- Registration
              Nothing          -- TestAgent
              (\_ _ _ _ _ _ _ s -> s) -- UserLoggedIn

-- Produces a new status expect that the status message is cleared.
clearStatus :: UserState -> UserState
clearStatus = Lens.set status Nothing

setLanguage :: Language -> UserState -> UserState
setLanguage = Lens.set language

getLanguage :: UserState -> Maybe Language
getLanguage st = st ^? language

getTimeZone :: UserState -> Maybe TimeZoneName
getTimeZone st = st ^? timeZone

setTimeZone :: TimeZoneName -> UserState -> UserState
setTimeZone = Lens.set timeZone

-- | Returns a username stored in the user state, or a description
--   string for the state
usernameInState :: UserState -> Username
usernameInState =
  userStateCata
    (const (Username "NotLoggedIn"))
    (Username "Registration")
    (Username "TestAgent")
    (\user _ _ _ _ _ _ _ -> user)

instance InRole UserState where
  isAdmin = userStateCata (const False) False False (\_ _ _ _ role _ _ _ -> isAdmin role)
  isCourseAdmin = userStateCata (const False) False False (\_ _ _ _ role _ _ _ -> Entities.isCourseAdmin role)
  isGroupAdmin = userStateCata (const False) False False (\_ _ _ _ role _ _ _ -> isGroupAdmin role)
  isStudent = userStateCata (const False) False False (\_ _ _ _ role _ _ _ -> isStudent role)

uidInState :: UserState -> Uid
uidInState =
  userStateCata
    (const (Uid "NotLoggedIn"))
    (Uid "Registration")
    (Uid "TestAgent")
    (\_ uid _ _ _ _ _ _ -> uid)

fullNameInState :: UserState -> String
fullNameInState =
  userStateCata
    (const "Not logged in")
    "Registration"
    "Test Agent"
    (\_ _ name _ _ _ _ _ -> name)

data ServiceContext = ServiceContext {
    logger             :: Logger
  , persistInterpreter :: Persist.Interpreter
  }

serviceContext :: Logger -> Persist.Interpreter -> ServiceContext
serviceContext = ServiceContext

