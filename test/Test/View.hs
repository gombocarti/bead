{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.View (
    tests
  ) where

import           Bead.Config (readConfiguration, beadConfigFileName, userActionLogFile)
import           Bead.Controller.Logging (mockLogger)
import           Bead.Controller.Pages (queueSubmissionForTest, queueAllSubmissionsForTest)
import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.ServiceContext (ServiceContext(ServiceContext))
import           Bead.Daemon.Email (startEmailDaemon)
import           Bead.Daemon.LDAP (LDAPDaemon(LDAPDaemon), LDAPResult(LDAPInvalidUser))
import           Bead.Domain.Entities
import           Bead.Domain.Relationships (SubmissionKey, AssignmentKey)
import           Bead.View.AuthToken (Cookie(..), AuthTokenManager(encryptCookie), createAuthTokenManager)
import           Bead.View.BeadContext (BeadContext)
import           Bead.View.BeadContextInit (beadContextInit, Daemons(Daemons))
import           Bead.View.Logger (createSnapLogger, snapLogger)
import           Bead.View.RequestParams (ReqParam(ReqParam))
import           Bead.View.RouteOf (pageRoutePath, pageRequestParams, routeOf)
import           Bead.View.Routing (pages)
import           Bead.Persistence.Persist (Persist)
import qualified Bead.Persistence.Persist as P
import qualified Bead.Persistence.Relations as P
import qualified Test.Property.Persistence as Prop

import           Control.Monad (forM_, forM, foldM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bifunctor (bimap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Either.Utils (forceEitherMsg)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Ord (Down(Down))
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import           Snap.Core (Response)
import qualified Snap.Core as Snap
import           Snap.Test (addHeader, assertRedirectTo, assertBodyContains, assertSuccess)
import qualified Snap.Test as Snap
import           Snap.Snaplet (SnapletInit, Snaplet)
import           Snap.Snaplet.Test (InitializerState)
import qualified Snap.Snaplet.Test as Snap
import           System.Directory (getTemporaryDirectory)
import           Test.QuickCheck (generate, elements, suchThat)
import           Test.Tasty (TestTree)
import           Test.Tasty.TestSet (TestSet, test, ioTest, group)
import           Test.Tasty.HUnit (testCase, assertEqual, Assertion)

tests :: TestSet ()
tests = group "View tests" $ do
  ioTest "Init persist interpreter" Prop.createInterpreter
  test queueSubmissionsForTest

initBead :: IO (SnapletInit BeadContext BeadContext, P.Interpreter)
initBead = do
  config <- readConfiguration beadConfigFileName
  p <- P.createPersistInterpreter P.defaultConfig
  email <- startEmailDaemon mockLogger
  tmp <- getTemporaryDirectory
#ifdef SSO
  let ldap = LDAPDaemon (const (return . return $ LDAPInvalidUser))
      daemons = Daemons email ldap
#else
  let daemons = Daemons email
#endif
  return (beadContextInit config (ServiceContext mockLogger p) daemons tmp, p)


-- Checks the following relationship holds:
-- User              |                        Assignment                            |
--                   |  GroupA1  |  GroupA2  |  Group B  |   CourseA   |   CourseB  |
-- course admin A    |  allowed  |  allowed  |           |   allowed   |            |
-- group admin A     |  allowed  |           |           |   allowed   |            |
-- student           |           |           |           |             |            |
-- logged out        |           |           |           |             |            |
--
queueSubmissionsForTest :: TestTree
queueSubmissionsForTest = testCase "Test queuing submissions for test" $ do
  Prop.reinitPersistence
  (beadInit, p) <- initBead
  bead <- forceEitherMsg "Failed to initialize BE-AD" <$> Snap.getSnaplet Nothing beadInit
  auth <- createAuthTokenManager
  -- generate data
  -- make sure all groups have assignment and all assignments have test case
  [c1, c2] <- Prop.courses 2
  gs1@[c1g1, c1g2] <- Prop.groups 2 [c1]
  gs2@[c2g1, c2g2] <- Prop.groups 2 [c2]
  unames <- Prop.users 10
  us@(u1:u2:u3:_) <- P.runPersistIOCmd p (mapM P.loadUser unames)
  let cAdmin = u1 { u_role = CourseAdmin }
      gAdmin = u2 { u_role = GroupAdmin }
      student = u3 { u_role = Student }
  P.runPersistIOCmd p $ do
    P.updateUser cAdmin
    P.updateUser gAdmin
    P.updateUser student
    P.createCourseAdmin (u_username cAdmin) c1
    P.createGroupAdmin (u_username cAdmin) c1g1
    P.createGroupAdmin (u_username gAdmin) c1g1
    P.subscribe (u_username student) c1g1
  subscriptions <- Prop.subscribeUsers 20 unames (gs1 ++ gs2)
  c1As <- Prop.courseAssignmentGen 3 [c1]
  c1g1As <- Prop.groupAssignmentGen 3 [c1g1]
  c1g2As <- Prop.groupAssignmentGen 3 [c1g2]
  c2g1As <- Prop.groupAssignmentGen 3 [c2g1]
  c2g2As <- Prop.groupAssignmentGen 3 [c2g2]
  c2As <- Prop.courseAssignmentGen 3 [c2]
  let as = concat [c1As, c1g1As, c1g2As, c2As, c2g1As, c2g2As]
  allSubms <- Prop.submissions 200 unames as
  lastSubms <- P.runPersistIOCmd p $ forM as $ \a -> (,) a <$> lastSubmissions a
  ts <- Prop.testScripts 4 [c1, c2]
  _ <- mapM (\a -> Prop.testCases 1 ts [a]) as
  uuid <- nextRandom
  let submissionsOf ak = [sk | (ak', sks) <- lastSubms, ak' == ak, sk <- sks]
      assignmentWithSubmission as = suchThat (elements as) (not . null . submissionsOf)
      pickAk as = generate $ assignmentWithSubmission as
      pickSk as = generate $ do
        a <- assignmentWithSubmission as
        elements $ submissionsOf a
      assertErrorPage resp = assertSuccess resp >> assertBodyContains "not administrated by you" resp
      assertUnaccessiblePage = assertRedirectTo (routeOf $ Pages.home ())
      assertNotLoggedIn = assertRedirectTo (routeOf $ Pages.index ())
  -- single submission tests
  --   course admin tests
  sk <- pickSk c1g1As
  testScenario
    "course admin queues a submission of a (group assignment, own group, own course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie cAdmin uuid))
    (HashSet.singleton sk)
    (assertRedirectTo (routeOf $ Pages.evaluation sk ()))
    bead
    p
  sk <- pickSk c1g2As
  testScenario
    "course admin queues a submission of a (group assignment, other group, own course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie cAdmin uuid))
    (HashSet.singleton sk)
    (assertRedirectTo (routeOf $ Pages.evaluation sk ()))
    bead
    p
  sk <- pickSk c1As
  testScenario
    "course admin queues a submission of a (course assignment, own course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie cAdmin uuid))
    (HashSet.singleton sk)
    (assertRedirectTo (routeOf $ Pages.evaluation sk ()))
    bead
    p
  sk <- pickSk c2g1As
  testScenario
    "course admin queues a submission of a (group assignment, other course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie cAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  sk <- pickSk c2As
  testScenario
    "course admin queues a submission of a (course assignment, other course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie cAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  --   group admin tests
  sk <- pickSk c1g1As
  testScenario
    "group admin queues a submission of a (group assignment, own group, own course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie gAdmin uuid))
    (HashSet.singleton sk)
    (assertRedirectTo (routeOf $ Pages.evaluation sk ()))
    bead
    p
  sk <- pickSk c1As
  testScenario
    "group admin queues a submission of a (course assignment, own course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie gAdmin uuid))
    (HashSet.singleton sk)
    (assertRedirectTo (routeOf $ Pages.evaluation sk ()))
    bead
    p
  sk <- pickSk c1g2As
  testScenario
    "group admin queues a submission of a (group assignment, other group, own course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie gAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  sk <- pickSk c2g1As
  testScenario
    "group admin queues a submission of a (group assignment, other group, other course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie gAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  sk <- pickSk c2As
  testScenario
    "group admin queues a submission of a (course assignment, other course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie gAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  --   student tests
  sk <- pickSk c1g1As
  testScenario
    "student queues a submission of a (group assignment, own group, own course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  sk <- pickSk c1g2As
  testScenario
    "student queues a submission of a (group assignment, other group, own course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  sk <- pickSk c1As
  testScenario
    "student queues a submission of a (course assignment, own course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  sk <- pickSk c2g1As
  testScenario
    "student queues a submission of a (group assignment, other group, other course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  sk <- pickSk c2As
  testScenario
    "student queues a submission of a (course assignment, other course)"
    (get (Pages.queueSubmissionForTest sk ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  --   logged out user tests
  sk <- pickSk c1g1As
  testScenario
    "logged out user queues a submission of a (group assignment)"
    (get (Pages.queueSubmissionForTest sk ()) auth Nothing)
    HashSet.empty
    assertNotLoggedIn
    bead
    p
  sk <- pickSk c1As
  testScenario
    "logged out user queues a submission of a (course assignment)"
    (get (Pages.queueSubmissionForTest sk ()) auth Nothing)
    HashSet.empty
    assertNotLoggedIn
    bead
    p
  ------
  -- bulk tests
  --   course admin tests
  ak <- pickAk c1g1As
  putStrLn $ "lasts in tests: " ++ show (submissionsOf ak) ++ " " ++ show ak
  testScenario
    "course admin queues submissions of a (group assignment, own group, own course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie cAdmin uuid))
    (HashSet.fromList (submissionsOf ak))
    (assertRedirectTo (routeOf $ Pages.home ()))
    bead
    p
  ak <- pickAk c1g2As
  putStrLn $ "lasts in tests: " ++ show (submissionsOf ak) ++ " " ++ show ak
  testScenario
    "course admin queues submissions of a (group assignment, other group, own course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie cAdmin uuid))
    (HashSet.fromList (submissionsOf ak))
    (assertRedirectTo (routeOf $ Pages.home ()))
    bead
    p
  ak <- pickAk c1As
  testScenario
    "course admin queues submissions of a (course assignment, own course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie cAdmin uuid))
    (HashSet.fromList (submissionsOf ak))
    (assertRedirectTo (routeOf $ Pages.home ()))
    bead
    p
  ak <- pickAk c2g1As
  testScenario
    "course admin queues submissions of a (group assignment, other course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie cAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  ak <- pickAk c2As
  testScenario
    "course admin queues submissions of a (course assignment, other course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie cAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  --   group admin tests
  ak <- pickAk c1g1As
  testScenario
    "group admin queues submissions of a (group assignment, own group, own course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie gAdmin uuid))
    (HashSet.fromList (submissionsOf ak))
    (assertRedirectTo (routeOf $ Pages.home ()))
    bead
    p
  ak <- pickAk c1As
  testScenario
    "group admin queues submissions of a (course assignment, own course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie gAdmin uuid))
    (HashSet.fromList (submissionsOf ak))
    (assertRedirectTo (routeOf $ Pages.home ()))
    bead
    p
  ak <- pickAk c1g2As
  testScenario
    "group admin queues submissions of a (group assignment, other group, own course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie gAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  ak <- pickAk c2g1As
  testScenario
    "group admin queues submissions of a (group assignment, other group, other course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie gAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  ak <- pickAk c2As
  testScenario
    "group admin queues submissions of a (course assignment, other course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie gAdmin uuid))
    HashSet.empty
    assertErrorPage
    bead
    p
  --   student tests
  ak <- pickAk c1g1As
  testScenario
    "student queues submissions of a (group assignment, own group, own course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  ak <- pickAk c1g2As
  testScenario
    "student queues submissions of a (group assignment, other group, own course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  ak <- pickAk c1As
  testScenario
    "student queues submissions of a (course assignment, own course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  ak <- pickAk c2g1As
  testScenario
    "student queues submissions of a (group assignment, other group, other course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  ak <- pickAk c2As
  testScenario
    "student queues submissions of a (course assignment, other course)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth (Just $ cookie student uuid))
    HashSet.empty
    assertUnaccessiblePage
    bead
    p
  --   logged out user tests
  ak <- pickAk c1g1As
  testScenario
    "logged out user queues submissions of a (group assignment)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth Nothing)
    HashSet.empty
    assertNotLoggedIn
    bead
    p
  ak <- pickAk c1As
  testScenario
    "logged out user queues submissions of a (course assignment)"
    (get (Pages.queueAllSubmissionsForTest ak ()) auth Nothing)
    HashSet.empty
    assertNotLoggedIn
    bead
    p
  Snap.closeSnaplet (snd bead)
  where
    testScenario :: String
                 -> Snap.RequestBuilder IO ()
                 -> HashSet SubmissionKey
                 -> (Response -> Assertion)
                 -> (Snaplet BeadContext, InitializerState BeadContext)
                 -> P.Interpreter
                 -> IO ()
    testScenario scenario req affectedSubms expectation bead p = do
      putStrLn $ "Test when " ++ scenario
      allSubm <- P.runPersistIOCmd p P.submissionKeys
      let unAffectedSubms = HashSet.difference (HashSet.fromList allSubm) affectedSubms
      allFs <- forM allSubm (\sk -> (,) sk <$> getFeedbacks sk)
      resp <- Snap.runHandler' (fst bead) (snd bead) req pages
      expectation (forceEitherMsg ("Error when " ++ scenario) resp)
      forM_ (HashSet.toList affectedSubms) $ \sk -> do
        let fs = lookup sk allFs
        fs' <- getFeedbacks sk
        assertEqual
          ("When " ++ scenario ++ ", a submission didn't get exactly one new feedback.")
          (((QueuedForTest :) . map info) <$> fs)
          (Just $ map info fs')
      forM_ (HashSet.toList unAffectedSubms) $ \sk -> do
        let fs = lookup sk allFs
        fs' <- getFeedbacks sk
        assertEqual
          ("When " ++ scenario ++ ", a submission got new feedback.")
          fs
          (Just fs')
        where
          getFeedbacks :: SubmissionKey -> IO [Feedback]
          getFeedbacks sk = P.runPersistIOCmd p $
            sortOn postDate <$> (P.feedbacksOfSubmission sk >>= mapM P.loadFeedback)
     

    cookie :: User -> UUID -> Cookie
    cookie u uuid = userCata
                      (\role username email name tz lang uid ->
                          LoggedInCookie {
                              cookieUsername = username
                            , cookieUid = uid
                            , cookieName = name
                            , cookieLanguage = lang
                            , cookieRole = role
                            , cookieUuid = uuid
                            , cookieTimezone = tz
                            , cookieStatus = Nothing
                            })
                      u
    addCookie :: AuthTokenManager -> Cookie -> Snap.RequestBuilder IO ()
    addCookie auth cookie = liftIO (cookieToSnapCookie "token" cookie) >>= Snap.addCookies . (:[])
      where
        cookieToSnapCookie :: ByteString -> Cookie -> IO Snap.Cookie
        cookieToSnapCookie name c = do
          encrypted <- forceEitherMsg "Error while encrypting a cookie" <$> liftIO (encryptCookie auth cookie)
          return $ Snap.Cookie {
              Snap.cookieName  = name
            , Snap.cookieValue = encrypted
            , Snap.cookieExpires = Nothing
            , Snap.cookieDomain = Nothing
            , Snap.cookiePath = Nothing
            , Snap.cookieSecure = False
            , Snap.cookieHttpOnly = True
            }

    get page auth mCookie = maybe id (\c -> (>> addCookie auth c)) mCookie (Snap.get path (Map.fromList params))
      where
        path = Pages.pageValue (pageRoutePath page)
        params = [(B.pack k, [B.pack v]) | ReqParam (k, v) <- Pages.pageValue (pageRequestParams page)]
                                                                     
    lastSubmissions :: AssignmentKey -> Persist [SubmissionKey]
    lastSubmissions ak = do
      ckGk <- P.courseOrGroupOfAssignment ak
      users <- either P.subscribedToCourse P.subscribedToGroup ckGk
      catMaybes <$> mapM (P.lastSubmission ak) users
