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
import           Snap.Snaplet.Fay
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
  , [ (markdownPath, serveMarkdown) ]
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
      result <- withUserState $ \state ->
        getPageContents
        (\err -> do
            logMessage ERROR $ "Error happened during log in: " ++ contentHandlerErrorMsg err
            translationErrorPage
              (msg_Login_PageTitle "Login")
              (msg_Login_InternalError "Some internal error happened, please contact the administrators."))
        (do result <- loginSubmit
            beadHandler $ traverse (bootstrapPublicPage defaultPageSettings) result)
        (SC.userNotLoggedIn (fromMaybe lang (SC.getLanguage state)))
      either (redirect . routeOf) serve result

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
redirectToParentPage :: P.PageDesc -> ContentHandler (PageContents H.Html)
redirectToParentPage = redirectTo . fromMaybe (P.home ()) . P.parentPage

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

-- Runs the 'h' handler if no error occurs during the run of the
-- handler calculates the parent page for the given 'p', and runs the
-- attached userstory from the calculated user action and redirects to
-- the parent page at the end, otherwise runs the onError handler.
runAction
  :: (ContentError -> BeadHandler H.Html)
  -> P.PageDesc
  -> ContentHandler UserAction
  -> SC.UserState
  -> BeadHandler (PageContents H.Html, SC.UserState)
runAction onError p h uState
  = evalHandlerError
      ((Right <$>) . onError)
      return
      (do userAction <- h
          userStory $ userStoryFor userAction
          redirectToParentPage p)
      uState

getPageContents
  :: (ContentError -> BeadHandler H.Html)
  -> ContentHandler (PageContents H.Html)
  -> SC.UserState
  -> BeadHandler (PageContents H.Html, SC.UserState)
getPageContents onError ch uState
  = evalHandlerError
      ((Right <$>) . onError)
      return
      ch
      uState

getData
  :: (ContentError -> BeadHandler H.Html)
  -> ContentHandler File
  -> SC.UserState
  -> BeadHandler (Either H.Html File, SC.UserState)
getData onError ch uState
  = evalHandlerError
      ((Left <$>) . onError)
      (return . Right)
      ch
      uState

data Response
  = Html H.Html
  | Redirection RedirectionTarget
  | File File

responseCata :: (H.Html -> a) -> (RedirectionTarget -> a) -> (File -> a) -> Response -> a
responseCata html redirection file response =
  case response of
    Html h -> html h
    Redirection page -> redirection page
    File f -> file f

-- Helper type synonyms

type CH     = ContentHandler (PageContents H.Html)
type CHUA   = ContentHandler UserAction
type CHData = ContentHandler File

type PageRenderer = P.Page CH CH (CH,CHUA) CHUA CHData

renderResponse :: PageHandler -> PageRenderer
renderResponse p = P.pfmap
  (viewHandlerCata (>>= \result -> traverse addBootstrap result))
  (userViewHandlerCata (>>= \result -> traverse addBootstrap result))
  (viewModifyHandlerCata (\get post -> (get >>= \result -> traverse addBootstrap result, post)))
  (modifyHandlerCata Prelude.id)
  (dataHandlerCata Prelude.id)
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
handlePage page = P.pageKindCata view userView viewModify modify data_ page where
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

  serveContents :: PageContents H.Html -> BeadHandler ()
  serveContents = either (redirect . routeOf) serve

  serveResponse :: Response -> BeadHandler ()
  serveResponse = responseCata
                    serve
                    (redirect . routeOf)
                    downloadFile

  getContentsOrError :: ContentHandler (PageContents H.Html)
                     -> UserState
                     -> BeadHandler (PageContents H.Html, SC.UserState)
  getContentsOrError = getPageContents defErrorPage  --TODO I18N

  getDataOrError :: ContentHandler File
                 -> UserState
                 -> BeadHandler (Either H.Html File, SC.UserState)
  getDataOrError = getData defErrorPage

  runActionOrError :: ContentHandler UserAction
                   -> SC.UserState
                   -> BeadHandler (PageContents H.Html, SC.UserState)
  runActionOrError = runAction defErrorPage pageDesc

  contentsToResponse :: PageContents H.Html -> Response
  contentsToResponse = either Redirection Html

  dataToResponse :: Either H.Html File -> Response
  dataToResponse = either Html File

  view :: P.ViewPage (ContentHandler (PageContents H.Html)) -> BeadHandler ()
  view p = get . serveResponse =<< loggedInFilter (\s -> checkClearance s (first contentsToResponse <$> getContentsOrError (P.viewPageValue p) s) (notAllowed s))

  data_ :: P.DataPage (ContentHandler File) -> BeadHandler ()
  data_ p = get . serveResponse =<< loggedInFilter (\s -> checkClearance s (first dataToResponse <$> getDataOrError (P.dataPageValue p) s) (notAllowed s))
  
  userView :: P.UserViewPage (ContentHandler (PageContents H.Html)) -> BeadHandler ()
  userView p = post . serveResponse =<< loggedInFilter (\s -> checkClearance s ((first contentsToResponse) <$> getContentsOrError (P.userViewPageValue p) s) (notAllowed s))

  viewModify :: P.ViewModifyPage (ContentHandler (PageContents H.Html), ContentHandler UserAction)
             -> BeadHandler ()
  viewModify = (\(get, post) ->
                   getPost
                     (serveResponse =<< loggedInFilter (\s -> checkClearance s ((first contentsToResponse) <$> getContentsOrError get s) (notAllowed s)))
                     (serveResponse =<< loggedInFilter (\s -> checkClearance s ((first contentsToResponse) <$> runActionOrError post s) (notAllowed s))))
                 . P.viewModifyPageValue

  modify :: P.ModifyPage (ContentHandler UserAction) -> BeadHandler ()
  modify p = post . serveResponse =<< loggedInFilter (\s -> checkClearance s (first contentsToResponse <$> runActionOrError (P.modifyPageValue p) s) (notAllowed s))

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
