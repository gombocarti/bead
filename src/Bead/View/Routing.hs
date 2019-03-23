{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Arrows #-}
module Bead.View.Routing (
    routes
  , pages
#ifdef TEST
  , routingTest
#endif
  ) where

import           Control.Arrow
import           Control.Monad (void)

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BC

import           Control.Monad.State (runStateT)
import           Data.Either (either)
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (fromString)
import qualified Data.Text as T
import           Prelude hiding (id)
import qualified Prelude
import qualified Text.Blaze.Html5 as H

import qualified Data.ByteString.Char8 as BS
import           Snap.Util.FileServe (serveDirectory)

import           Bead.Config (Config(..))
import           Bead.Controller.Logging as L
import qualified Bead.Controller.ServiceContext as SC
import qualified Bead.Controller.Pages as P
import qualified Bead.Controller.UserStories as S
import           Bead.Domain.Entities as E
import qualified Bead.View.AuthToken as Auth
import           Bead.View.BeadContext
import           Bead.View.Common
import           Bead.View.Content hiding (Response, BlazeTemplate, template, withUserState, getDictionaryInfos)
import           Bead.View.ContentHandler hiding (withUserState, getDictionaryInfos)
import qualified Bead.View.ContentHandler as ContentHandler
import           Bead.View.Content.All
import           Bead.View.ErrorPage
import           Bead.View.Login as L
import           Bead.View.Markdown
import           Bead.View.Pagelets
import qualified Bead.View.Content.Public.Index as I
#ifndef SSO
import           Bead.View.Registration
import           Bead.View.ResetPassword
#endif
import           Bead.View.RouteOf
import           Bead.View.RequestParams

#ifdef TEST
import           Test.Tasty.TestSet
#endif

{-
Routing of a given request based on the path of the request. A page handler
is selected of the path is known otherwise an error page is rendered.
-}

-- * Route table

routes :: Config -> [(ByteString, BeadHandler ())]
routes config = join
  [ -- Add login handlers
    [ (indexPath,          index)
    , (changeLanguagePath, changeLanguage)
    ]
  , registrationRoutes config
    -- Add static handlers
  , [ (staticPath,         serveDirectory "static") ]
  ]

registrationRoutes :: Config -> [(ByteString, BeadHandler ())]
#ifdef SSO
registrationRoutes _ = []
#else
registrationRoutes config = [
    ("/reset_pwd",resetPasswordPage)
  , ("/reg_request", registrationRequest config)
  , ("/reg_final", finalizeRegistration)
  ]
#endif

pages :: BeadHandler ()
pages = do
  path <- getRequest >>= return . proc req -> do
            ctx <- rqContextPath -< req
            pth <- rqPathInfo    -< req
            returnA -< (BS.append ctx pth)
  page <- requestToPageHandler path
  case page of
    -- No Page value is calculated from the request, pass to other handler
    Nothing -> pass
    Just pd
      | P.isLogin pd -> handleLogin
      | otherwise -> handlePage $ renderResponse $ pageContent pd
  where
    -- Logs the user in, invalidating its previous logins (if any)
    -- but keeps the language information (if any).
    handleLogin :: BeadHandler ()
    handleLogin = do
      lang <- configuredDefaultDictionaryLanguage
      response <- withUserState $ \state ->
        evalHandlerError
        (\err -> do
            logMessage ERROR $ "Error happened during log in: " ++ contentHandlerErrorMsg err
            Html <$>
              translationErrorPage
                (msg_Login_PageTitle "Login")
                (msg_Login_InternalError "Some internal error happened, please contact the administrators."))
        (return . contentsToResponse)
        (do result <- loginSubmit
            beadHandler $ traverse (bootstrapPublicPage defaultPageSettings) result)
        (SC.userNotLoggedIn (fromMaybe lang (SC.getLanguage state)))
      serveResponse response

serve :: H.Html -> BeadHandler ()
serve = blaze

downloadFile :: File -> BeadHandler ()
downloadFile (fname, mime, writeContents) = do
  modifyResponse $
    setHeader "Content-Disposition" (fromString $ concat ["attachment; filename=\"", escapeQuotes fname,"\""])
  modifyResponse $
    setHeader "Content-Type" (fromString contentType)
  writeContents
  where
    contentType :: String
    contentType = ContentHandler.mimeCata
      "application/zip, application/octet-stream"
      "text/plain; charset=\"UTF-8\""
      mime

    escapeQuotes :: String -> String
    escapeQuotes = concatMap escape
      where
        escape :: Char -> String
        escape '\"' = "\\\""
        escape c    = [c]

-- * Handlers

index :: BeadHandler ()
#ifdef SSO
index =
  ifTop $ requireUser
            (redirect (routeOf $ P.home ()))
            (I.index Nothing >>= bootstrapPublicPage defaultPageSettings >>= serve)
#else
index =
  ifTop $ requireUser
            (redirect (routeOf $ P.home ()))
            (login Nothing)
#endif
  where
    requireUser :: BeadHandler () -> BeadHandler () -> BeadHandler ()
    requireUser authenticated unauthenticated = do
      eCookie <- getCookie
      either
        handleCookieError
        (\cookie -> if Auth.isLoggedIn cookie
                    then authenticated
                    else unauthenticated)
        eCookie

        where
          handleCookieError :: T.Text -> BeadHandler ()
          handleCookieError err = do
            logMessage ERROR ("Cookie reading error: " ++ T.unpack err)
            unauthenticated

-- | Change language in the session
changeLanguage :: BeadHandler ()
changeLanguage = method GET handler <|> method POST (redirect indexPath)
  where
    handler :: BeadHandler ()
    handler = do
      withUserState $
        evalHandlerError logError return setLanguage
      redirect $ routeOf $ P.index ()

    setLanguage :: ContentHandler ()
    setLanguage = setUserLanguage =<< getParameter changeLanguagePrm

    logError :: ContentError -> BeadHandler ()
    logError err = 
      logMessage ERROR ("Change language: " ++ contentHandlerErrorMsg err)

-- Redirects to the parent page of the given page
redirectToParentPage :: P.PageDesc -> Response
redirectToParentPage = Redirection . fromMaybe (P.home ()) . P.parentPage

contentsToResponse :: PageContents H.Html -> Response
contentsToResponse = either Redirection Html

evalHandlerError
  :: (ContentError -> BeadHandler a)
  -> (c -> BeadHandler a)
  -> ContentHandler c
  -> SC.UserState
  -> BeadHandler (a, SC.UserState)
evalHandlerError onError onSuccess h uState = do
  (x, (uState', _pageSettings)) <- runStateT (runExceptT h) (uState, E.defaultPageSettings)
  addState uState' $
    case x of
      Left e  -> onError e
      Right s -> onSuccess s

  where
    addState :: UserState -> BeadHandler a -> BeadHandler (a, UserState)
    addState s h = (,) <$> h <*> pure s

type HttpStatusCode = Int

data Response
  = Html H.Html
  | Redirection RedirectionTarget
  | File File
  | Json Aeson.Encoding
  | ReturnCode HttpStatusCode

responseCata :: (H.Html -> a) -> (RedirectionTarget -> a) -> (File -> a) -> (Aeson.Encoding -> a) -> (HttpStatusCode -> a) -> Response -> a
responseCata html redirection file json returnCode response =
  case response of
    Html h -> html h
    Redirection page -> redirection page
    File f -> file f
    Json j -> json j
    ReturnCode n -> returnCode n

serveResponse :: Response -> BeadHandler ()
serveResponse = responseCata
                  serve
                  (redirect . routeOf)
                  downloadFile
                  serveJson
                  setStatusCode

  where
    setStatusCode :: HttpStatusCode -> BeadHandler ()
    setStatusCode = modifyResponse . setResponseCode

    serveJson :: Aeson.Encoding -> BeadHandler ()
    serveJson = writeBuilder . Aeson.fromEncoding

-- Helper type synonyms

type CH     = ContentHandler (PageContents H.Html)
type CHUA   = ContentHandler UserAction
type CHData = ContentHandler File
type CHRest = ContentHandler Aeson.Encoding

type PageRenderer = P.Page CH CH (CH,CHUA) CHUA CHData CHRest

renderResponse :: PageHandler -> PageRenderer
renderResponse p = P.pfmap
  (viewHandlerCata (>>= \result -> traverse addBootstrap result))
  (userViewHandlerCata (>>= \result -> traverse addBootstrap result))
  (viewModifyHandlerCata (\get post -> (get >>= \result -> traverse addBootstrap result, post)))
  (modifyHandlerCata Prelude.id)
  (dataHandlerCata Prelude.id)
  (restViewHandlerCata Prelude.id)
  p

  where
    addBootstrap :: IHtml -> ContentHandler H.Html
    addBootstrap contents = bootstrapPage (P.setPageValue contents p)

{- When a user logs in the home page is shown for her. An universal handler
   is used. E.g "/home" -> handlePage P.Home.
   * If the user can navigate to the
   intended page from its state, it's state is going to change in his session
   and in the server side as well.
   * When a user submits information with a POST request, from the submitted information
   we calculate the appropiate user action and run it
-}
handlePage :: PageRenderer -> BeadHandler ()
handlePage page = P.pageKindCata view userView viewModify modify data_ restView page where
  pageDesc = P.pageToPageDesc page

  invalidPOSTMethodCall :: BeadHandler ()
  invalidPOSTMethodCall = do
     logMessage DEBUG $ "Invalid POST handler " ++ show pageDesc
     redirect homePath

  invalidGETMethodCall :: BeadHandler ()
  invalidGETMethodCall = do
     logMessage DEBUG $ "Invalid GET handler" ++ show pageDesc
     redirect homePath

  checkClearance :: UserState -> a -> a -> a
  checkClearance s handler voilationHandler =
    if P.allowedPage (role s) page
    then handler
    else voilationHandler

  notAllowed :: UserState -> BeadHandler (Response, UserState)
  notAllowed s = do
    logMessage ERROR . join $ [
        usernameCata Prelude.id (SC.usernameInState s)
      , ": Loading of page "
      , show pageDesc
      , " is not allowed"
      ]
    return (Redirection (P.home ()), s)

  get :: BeadHandler () -> BeadHandler ()
  get h = method GET h <|> method POST invalidPOSTMethodCall

  post :: BeadHandler () -> BeadHandler ()
  post h = method GET invalidGETMethodCall <|> method POST h

  getPost :: BeadHandler () -> BeadHandler () -> BeadHandler ()
  getPost g p = method GET g <|> method POST p

  getContentsOrError :: ContentHandler (PageContents H.Html) -> BeadHandler ()
  getContentsOrError handler = do
    response <- loggedInFilter $ \s ->
      checkClearance
        s
        (evalHandlerError (\err -> Html <$> defErrorPage err) (return . contentsToResponse) handler s)
        (notAllowed s)
    serveResponse response

  getFileOrError :: ContentHandler File -> BeadHandler ()
  getFileOrError handler = do
    response <- loggedInFilter $ \s ->
      checkClearance
        s
        (evalHandlerError (\err -> Html <$> defErrorPage err) (return . File) handler s)
        (notAllowed s)
    serveResponse response

  -- Runs the 'handler' handler if no error occurs during the run of the
  -- handler calculates the parent page for the given 'p', and runs the
  -- attached userstory from the calculated user action and redirects to
  -- the parent page at the end, otherwise runs the onError handler.
  runActionOrError :: ContentHandler UserAction -> BeadHandler ()
  runActionOrError handler = do
    response <- loggedInFilter $ \s ->
      checkClearance
        s
        (evalHandlerError (\err -> Html <$> defErrorPage err) return runAction s)
        (notAllowed s)
    serveResponse response

      where
        runAction :: ContentHandler Response
        runAction = do
          userAction <- handler
          userStory $ userStoryFor userAction
          return $ redirectToParentPage pageDesc

  getJsonOrError :: ContentHandler Aeson.Encoding -> BeadHandler ()
  getJsonOrError handler = do
    response <- loggedInFilter $ \s ->
      checkClearance
        s
        (evalHandlerError (\err -> Html <$> defErrorPage err) (return . Json) handler s)
        (notAllowed s)
    serveResponse response

  view :: P.ViewPage (ContentHandler (PageContents H.Html)) -> BeadHandler ()
  view = get . getContentsOrError . P.viewPageValue

  data_ :: P.DataPage (ContentHandler File) -> BeadHandler ()
  data_ = get . getFileOrError . P.dataPageValue

  userView :: P.UserViewPage (ContentHandler (PageContents H.Html)) -> BeadHandler ()
  userView = post . getContentsOrError . P.userViewPageValue

  viewModify :: P.ViewModifyPage (ContentHandler (PageContents H.Html), ContentHandler UserAction) -> BeadHandler ()
  viewModify = (\(get, post) -> getPost (getContentsOrError get) (runActionOrError post)) . P.viewModifyPageValue

  modify :: P.ModifyPage (ContentHandler UserAction) -> BeadHandler ()
  modify = post . runActionOrError . P.modifyPageValue

  restView :: P.RestViewPage (ContentHandler Aeson.Encoding) -> BeadHandler ()
  restView = get . getJsonOrError . P.restViewPageValue

withUserState :: (UserState -> BeadHandler (a, UserState)) -> BeadHandler a
withUserState f = do
  eCookieData <- getCookie
  state <- case eCookieData of
             Left err -> do
               logMessage ERROR ("Cookie reading error: " ++ T.unpack err)
               defaultUserState
             Right cookie ->
               return $ cookieToState cookie
  (a, state') <- f state
  when (state' /= state) $ saveState state'
  return a
  where
    cookieToState :: Auth.Cookie -> SC.UserState
    cookieToState = Auth.cookieCata
                      SC.UserNotLoggedIn
                      SC.UserLoggedIn

    saveState :: SC.UserState -> BeadHandler ()
    saveState new = do
      cookie <- SC.userStateCata
                  (return . Auth.NotLoggedInCookie)
                  defaultCookie
                  defaultCookie
                  (\u ui n l r uuid tz s -> return $ Auth.LoggedInCookie u ui n l r uuid tz s)
                  new
      result <- setCookie cookie
      case result of
        Left err ->
          logMessage ERROR ("Cookie saving error: " ++ T.unpack err)
        Right _ ->
          return ()

loggedInFilter :: (UserState -> BeadHandler (Response, UserState)) -> BeadHandler Response
loggedInFilter f = withUserState $ \s ->
    SC.userStateKindCata
      (addState s notLoggedIn)   -- UserNotLoggedIn
      (addState s notLoggedIn)   -- Registration
      (addState s notLoggedIn)   -- TestAgent
      (f s)                      -- UserLoggedIn
      s
  where
    notLoggedIn :: BeadHandler Response
    notLoggedIn = return . Redirection $ P.index ()

    addState :: UserState -> BeadHandler a -> BeadHandler (a, UserState)
    addState s h = (,) <$> h <*> pure s

-- Creates a handler, that tries to calculate a Page value
-- from the requested route and the parameters of the request uri
requestToPageHandler :: RoutePath -> BeadHandler (Maybe P.PageDesc)
requestToPageHandler path = requestToPage path <$> getParams

-- Calculates a Just page if the route is a valid route path
-- and all the parameters were given is the params for the
-- routePath necesary for the Page value, otherwise Nothing
requestToPage :: RoutePath -> Params -> Maybe P.PageDesc
requestToPage path params = do
  page <- Map.lookup path routeToPageMap
  page params

routeToPageMap :: Map RoutePath (Params -> Maybe P.PageDesc)
routeToPageMap = Map.fromList [
    (indexPath       , j $ P.index ())
  , (loginPath       , j $ P.login ())
  , (logoutPath      , j $ P.logout ())
  , (homePath        , j $ P.home ())
  , (profilePath     , j $ P.profile ())
  , (courseAdminPath , j $ P.courseAdmin ())
  , (courseOverviewPath , \ps -> P.courseOverview <$> courseKey ps <*> unit)
  , (modifyEvaluationPath , \ps -> P.modifyEvaluation <$> submissionKey ps <*> evaluationKey ps <*> unit)
  , (evaluationTablePath  , j $ P.evaluationTable ())
  , (evaluationPath , \ps -> P.evaluation <$> submissionKey ps <*> unit)
  , (submissionPath , \ps -> P.submission <$> assignmentKey ps <*> unit)
  , (viewUserScorePath    , \ps -> P.viewUserScore <$> scoreKey ps <*> unit)
  , (newUserScorePath     , \ps -> P.newUserScore <$> assessmentKey ps <*> username ps <*> unit)
  , (modifyUserScorePath  , \ps -> P.modifyUserScore <$> scoreKey ps <*> unit)
  , (newTestScriptPath    , j $ P.newTestScript ())
  , (modifyTestScriptPath , \ps -> P.modifyTestScript <$> testScriptKey ps <*> unit)
  , (uploadFilePath , j $ P.uploadFile ())
  , (submissionDetailsPath , \ps -> P.submissionDetails <$> assignmentKey ps <*> submissionKey ps <*> unit)
  , (administrationPath    , j $ P.administration ())
  , (groupRegistrationPath , j $ P.groupRegistration ())
  , (createCoursePath      , j $ P.createCourse ())
  , (userDetailsPath       , j $ P.userDetails ())
  , (assignCourseAdminPath , j $ P.assignCourseAdmin ())
  , (createGroupPath       , j $ P.createGroup ())
  , (assignGroupAdminPath  , j $ P.assignGroupAdmin ())
  , (newGroupAssignmentPath , \ps -> P.newGroupAssignment <$> groupKey ps <*> unit)
  , (newCourseAssignmentPath , \ps -> P.newCourseAssignment <$> courseKey ps <*> unit)
  , (modifyAssignmentPath , \ps -> P.modifyAssignment <$> assignmentKey ps <*> unit)
  , (viewAssignmentPath , \ps -> P.viewAssignment <$> assignmentKey ps <*> unit)
  , (newGroupAssignmentPreviewPath , \ps -> P.newGroupAssignmentPreview <$> groupKey ps <*> unit)
  , (newCourseAssignmentPreviewPath , \ps -> P.newCourseAssignmentPreview <$> courseKey ps <*> unit)
  , (modifyAssignmentPreviewPath , \ps -> P.modifyAssignmentPreview <$> assignmentKey ps <*> unit)
  , (changePasswordPath      , j $ P.changePassword ())
#ifndef SSO
  , (setUserPasswordPath     , j $ P.setUserPassword ())
#endif
  , (deleteUsersFromCoursePath , \ps -> P.deleteUsersFromCourse <$> courseKey ps <*> unit)
  , (deleteUsersFromGroupPath , \ps -> P.deleteUsersFromGroup <$> groupKey ps <*> unit)
  , (unsubscribeFromCoursePath , \ps -> P.unsubscribeFromCourse <$> groupKey ps <*> unit)
  , (exportEvaluationsScoresPath, \ps -> P.exportEvaluationsScores <$> courseKey ps <*> unit)
  , (exportEvaluationsScoresAllGroupsPath, \ps -> P.exportEvaluationsScoresAllGroups <$> courseKey ps <*> unit)
  , (exportSubmissionsPath, \ps -> P.exportSubmissions <$> assignmentKey ps <*> unit)
  , (exportSubmissionsOfGroupsPath, \ps -> P.exportSubmissionsOfGroups <$> assignmentKey ps <*> username ps <*> unit)
  , (exportSubmissionsOfOneGroupPath, \ps -> P.exportSubmissionsOfOneGroup <$> assignmentKey ps <*> groupKey ps <*> unit)
  , (getSubmissionPath, \ps -> P.getSubmission <$> submissionKey ps <*> unit)
  , (getCourseCsvPath, \ps -> P.getCourseCsv <$> courseKey ps <*> unit)
  , (getGroupCsvPath, \ps -> P.getGroupCsv <$> groupKey ps <*> unit)
  , (newGroupAssessmentPath, \ps -> P.newGroupAssessment <$> groupKey ps <*> unit)
  , (newCourseAssessmentPath, \ps -> P.newCourseAssessment <$> courseKey ps <*> unit)
  , (fillNewGroupAssessmentPreviewPath, \ps -> P.fillNewGroupAssessmentPreview <$> groupKey ps <*> unit)
  , (fillNewCourseAssessmentPreviewPath, \ps -> P.fillNewCourseAssessmentPreview <$> courseKey ps <*> unit)
  , (modifyAssessmentPath, \ps -> P.modifyAssessment <$> assessmentKey ps <*> unit)
  , (modifyAssessmentPreviewPath, \ps -> P.modifyAssessmentPreview <$> assessmentKey ps <*> unit)
  , (viewAssessmentPath, \ps -> P.viewAssessment <$> assessmentKey ps <*> unit)
  , (notificationsPath, j $ P.notifications ())
  , (submissionTablePath, \ps -> P.submissionTable <$> groupKey ps <*> unit)
  ] where
      j = const . Just
      unit = return ()

      courseKey     = fmap (CourseKey     . unpack) . value courseKeyParamName
      groupKey      = fmap (GroupKey      . unpack) . value groupKeyParamName
      assignmentKey = fmap (AssignmentKey . unpack) . value assignmentKeyParamName
      submissionKey = fmap (SubmissionKey . unpack) . value submissionKeyParamName
      evaluationKey = fmap (EvaluationKey . unpack) . value evaluationKeyParamName
      testScriptKey = fmap (TestScriptKey . unpack) . value testScriptKeyParamName
      assessmentKey = fmap (AssessmentKey . unpack) . value assessmentKeyParamName
      scoreKey      = fmap (ScoreKey . unpack) . value scoreKeyParamName
      username      = fmap (Username . unpack) . value (fieldName usernameField)

      -- Returns Just x if only one x corresponds to the key in the request params
      -- otherwise Nothing
      value key params = Map.lookup key params >>= oneValue
        where
          oneValue [l] = Just l
          oneValue _   = Nothing

#ifdef TEST

requestToParams :: [ReqParam] -> Params
requestToParams = foldl insert Map.empty
  where
    insert m (ReqParam (name, value)) =
      Map.insert (fromString name) [(fromString value)] m

routingTest =
  assertProperty
    "requestToPage is totally defined"
    (\p -> let rp = P.pageValue $ pageRoutePath p
               ps = requestToParams . P.pageValue $ pageRequestParams p
           in requestToPage rp ps == Just p)
    P.pageGen
    "requestToPage must be defined for each page"


#endif
