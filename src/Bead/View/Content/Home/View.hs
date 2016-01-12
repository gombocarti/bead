{-# LANGUAGE OverloadedStrings, CPP #-}
module Bead.View.Content.Home.View where

import           Control.Arrow ((***))
import           Control.Monad.Identity
import           Data.Function (on)
import           Data.List (find, intersperse, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.String (fromString)

import           Text.Blaze.Html5 hiding (map, id)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A hiding (id)

import qualified Bead.Controller.Pages as Pages
import           Bead.Domain.Entities as E (Role(..))
import           Bead.Domain.Evaluation
import           Bead.View.Content as Content hiding (userState, table, assessments)
import           Bead.View.Content.SubmissionTable as ST
import           Bead.View.Content.VisualConstants

import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.Home.Data

homeContent :: HomePageData -> IHtml
homeContent d = do
  let s = userState d
      r = role s
      hasCourse = hasCourses d
      hasGroup  = hasGroups d
      testScripts = courseTestScripts d
  msg <- getI18N
  return $ do
            when (isAdmin s) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ do
                h3 . fromString . msg $ msg_Home_AdminTasks "Administrator Menu"
                i18n msg $ navigation [administration]

            -- Course Administration Menu
            when (courseAdminUser r) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ do
                h3 . fromString . msg $ msg_Home_CourseAdminTasks "Course Administrator Menu"
                when (not hasCourse) $ do
                  H.p $ fromString . msg $ msg_Home_NoCoursesYet
                    "There are no courses.  Contact the administrator to have courses assigned."

            -- Submission tables for course or group assignments
            when ((courseAdminUser r) || (groupAdminUser r)) $ do
              when hasGroup $ do
                when (not . null $ concatMap submissionTableInfoAssignments $ sTables d) $ do
                  Bootstrap.row $ Bootstrap.colMd12 $ p $ fromString . msg $ msg_Home_SubmissionTable_Info $ concat
                    [ "Assignments may be modified by clicking on their identifiers if you have rights for the modification (their names are shown in the tooltip).  "
                    , "Students may be unregistered from the courses or the groups by checking the boxes in the Remove column "
                    , "then clicking on the button."
                    ]
                i18n msg $ htmlSubmissionTables d

              -- HR
              Bootstrap.row $ Bootstrap.colMd12 $ hr

              -- Course Administration links
              when hasCourse $ do
                Bootstrap.row $ Bootstrap.colMd12 $ h3 $ fromString . msg $ msg_Home_CourseAdministration "Course Administration"
                Bootstrap.row $ Bootstrap.colMd12 $ fromString . msg $ msg_Home_CourseSubmissionTableList_Info $ concat
                  [ "Submission table for courses can be found on separate pages, please click on the "
                  , "name of a course."
                  ]
                Bootstrap.row $ Bootstrap.colMd12 $ ul ! class_ "list-group" $ do
                  let courseList = sortBy (compareHun `on` (courseName . snd)) $ Map.toList $ administratedCourseMap d
                  forM_ courseList $ \(ck, c) ->
                    li ! class_ "list-group-item"
                       $ a ! href (fromString $ routeOf (courseOverview ck))
                       $ (fromString (courseName c))

            -- Course Administration Button Group
            when (courseAdminUser r && hasCourse) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ p $ fromString . msg $ msg_Home_CourseAdministration_Info $ concat
                [ "New groups for courses may be created in the Course Settings menu.  Teachers may be also assigned to "
                , "each of the groups there as well."
                ]
              i18n msg $ navigation $ courseAdminButtons
            -- Group Administration Button Group
            when (groupAdminUser r && hasGroup) $ do
              i18n msg $ navigation groupAdminButtons

            -- HR
            when (or [groupAdminUser r && hasGroup, courseAdminUser r && hasCourse]) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ hr

            -- Student Menu
            when (not $ isAdmin r) $ do
              Bootstrap.row $ Bootstrap.colMd12 $ h3 $ fromString $ msg $ msg_Home_StudentTasks "Student Menu"
              i18n msg $ navigation [groupRegistration]
              i18n msg $ availableAssignments d (timeConverter d) (assignments d)
  where
      administration    = Pages.administration ()
      courseAdmin       = Pages.courseAdmin ()
      courseOverview ck = Pages.courseOverview ck ()
      evaluationTable   = Pages.evaluationTable ()
      groupRegistration = Pages.groupRegistration ()
      newTestScript     = Pages.newTestScript ()
#ifndef SSO
      setUserPassword   = Pages.setUserPassword ()
#endif
      submission     = Pages.submission ()
      submissionList = Pages.submissionList ()
      uploadFile     = Pages.uploadFile ()

      courseAdminUser = (==E.CourseAdmin)
      groupAdminUser  = (==E.GroupAdmin)

      -- With single sign-on, passwords cannot be set.
#ifdef SSO
      courseAdminButtons = [ courseAdmin, newTestScript, evaluationTable, uploadFile ]
      groupAdminButtons = [ evaluationTable, uploadFile ]
#else
      courseAdminButtons = [courseAdmin, newTestScript, evaluationTable, setUserPassword, uploadFile ]
      groupAdminButtons = [evaluationTable, setUserPassword, uploadFile ]
#endif

-- * Helpers

submissionTableInfoAssignments = submissionTableInfoCata course group where
  course _n _us as _uls _ans _ck = as
  group _n _us cgas _uls _ans _ck _gk = map (cgInfoCata id id) cgas

htmlSubmissionTables :: HomePageData -> IHtml
htmlSubmissionTables pd = do
  sbmTables <- mapM (htmlSubmissionTable pd) $ zip [1..] (sTables pd)
  asmtTables <- mapM (assessmentTable pd) (sTables pd)
  return $ forM_ (zip sbmTables asmtTables) $ \(s,a) -> s >> a
  where
    assessmentTable pd s = do
      case Map.lookup (submissionTableInfoToCourseGroupKey s) (assessmentTables pd) of
        Nothing -> return $ return ()
        Just sb -> htmlAssessmentTable sb

    htmlSubmissionTable pd (i,s) = do
      submissionTable (concat ["st", show i]) (now pd) (submissionTableCtx pd) s


htmlAssessmentTable :: ScoreBoard -> IHtml
htmlAssessmentTable board
  | (null . sbAssessments $ board) = return . Bootstrap.rowColMd12 . H.p $ "There are no assessments yet."
  | otherwise = do
      msg <- getI18N
      return $ do
        Bootstrap.rowColMd12 . H.p $ "Assessments"
        Bootstrap.rowColMd12 . Bootstrap.table $ do
          H.tr $ do
            H.th . string $ "Name"
            H.th . string $ "Username"
            forM_ (zip (sbAssessments board) [1..]) assessmentViewButton
          forM_ (sbUsers board) (userLine msg)
      where
        assessmentViewButton :: (AssessmentKey,Int) -> Html
        assessmentViewButton (ak,n) = H.td $ Bootstrap.customButtonLink style "" (assessmentName ak) ("A" ++ show n)
            where 
              style = [fst ST.groupButtonStyle]

        assessmentName :: AssessmentKey -> String
        assessmentName ak = maybe "" Content.title (Map.lookup ak (sbAssessmentInfos board))

        userLine :: I18N -> UserDesc -> Html
        userLine msg userDesc = H.tr $ do
          H.td . string . ud_fullname $ userDesc
          H.td . string . uid id . ud_uid $ userDesc 
          forM_ (sbAssessments board) (scoreIcon msg . ud_username $ userDesc)

        scoreIcon :: I18N -> Username -> AssessmentKey -> Html
        scoreIcon msg username ak = H.td $ case Map.lookup (ak,username) (sbScores board) of
          Just si -> scoreInfoToIcon msg ("notFoundLink") ("foundLink") si
          Nothing -> mempty

scoreInfoToIcon :: I18N -> String -> String -> ScoreInfo -> Html
scoreInfoToIcon msg notFoundLink foundLink =
  scoreInfoAlgebra (linkWithHtml notFoundLink notFound) $
    \ek -> (linkWithHtml foundLink . evResultCata (binaryCata (resultCata passed failed)) percentage free)
  where
    tooltip m = A.title (fromString $ msg m)
    notFound = (H.i ! A.class_ "glyphicon glyphicon-stop"  ! A.style "color:#AAAAAA; font-size: xx-large"
                    ! tooltip (msg_Home_SubmissionCell_NonEvaluated "Non evaluated") $ mempty)

    passed = (H.i ! A.class_ "glyphicon glyphicon-thumbs-up" ! A.style "color:#00FF00; font-size: xx-large"
                  ! tooltip (msg_Home_SubmissionCell_Accepted "Accepted") $ mempty) -- accepted

    failed = (H.i ! A.class_ "glyphicon glyphicon-thumbs-down" ! A.style "color:#FF0000; font-size: xx-large"
                  ! tooltip (msg_Home_SubmissionCell_Rejected "Rejected") $ mempty) -- rejected

    percentage (Percentage (Scores [p])) = H.span ! A.class_ "label label-primary" $ fromString $ percent p
    percentage _ = error "SubmissionTable.coloredSubmissionCell percentage is not defined"
    free = freeForm $ \msg ->
      let cell = if length msg < displayableFreeFormResultLength then msg else "..." in
      H.span ! A.class_ "label label-primary"
             ! A.title (fromString msg) $ (fromString cell)

    percent x = join [show . round $ (100 * x), "%"]

navigation :: [Pages.Page a b c d e] -> IHtml
navigation links = do
  msg <- getI18N
  return
    $ Bootstrap.row
    $ Bootstrap.colMd12
    $ H.div ! class_ "btn-group"
    $ mapM_ (i18n msg . linkButtonToPageBS) links

availableAssignments :: HomePageData -> UserTimeConverter -> StudentAssignments -> IHtml
availableAssignments pd timeconverter studentAssignments
  | isNotRegistered studentAssignments = do
      msg <- getI18N
      return
        $ Bootstrap.row
        $ Bootstrap.colMd12
        $ p
        $ fromString
        $ msg $ msg_Home_HasNoRegisteredCourses "There are no registered courses, register to some."

  | null (toAllActiveAssignmentList studentAssignments) = do
      msg <- getI18N
      return
        $ Bootstrap.row
        $ Bootstrap.colMd12
        $ p
        $ fromString
        $ msg $ msg_Home_HasNoAssignments "There are no available assignments yet."
        --TODO: Add assessment table here!!!

  | otherwise = do
      -- Sort course or groups by their name.
      let asl = sortBy (compare `on` snd3)
                  $ map courseName3
                  $ toActiveAssignmentList studentAssignments
      msg <- getI18N
      return $ do
        Bootstrap.row
          $ Bootstrap.colMd12
          $ p
          $ fromString . msg $ msg_Home_Assignments_Info $ concat
            [ "Submissions and their evaluations may be accessed by clicking on each assignment's link. "
            , "The table shows only the last evaluation per assignment."
            ]
        forM_ asl $ \(key, coursename, as) -> when (not $ null as) $ Bootstrap.rowColMd12 $ do
          h4 $ fromString coursename
          let areIsolateds = areOpenAndIsolatedAssignments as
          let assignments = if areIsolateds then (isolatedAssignments as) else as
          let isLimited = isLimitedAssignments assignments
          when areIsolateds $ p $ fromString . msg $ msg_Home_ThereIsIsolatedAssignment $ concat
            [ "ISOLATED MODE: There is at least one assignment which hides the normal assignments for "
            , "this course."
            ]
          Bootstrap.table $ do
            thead $ headerLine msg isLimited
            -- Sort assignments by their end date time in reverse
            tbody $ mapM_ (assignmentLine msg isLimited)
                  $ reverse $ sortBy (compare `on` (aEndDate . activeAsgDesc))
                  $ assignments
          -- Assessment table
          case Map.lookup key (assessments pd) of
            Nothing  -> p $ fromString "There are no assessments registered to this course"
            Just cas -> availableAssessment msg cas
  where
    snd3 (_,s,_) = s
    courseName3 (ck, c, as) = (ck, courseName c, as)
    isLimitedAssignments = isJust . find limited

    limited = submissionLimit (const False) (\_ _ -> True) (const True) . (\(_a,ad,_si) -> aLimit ad)

    isOpenAndIsolated a = and [aIsolated a, aActive a]

    areOpenAndIsolatedAssignments = isJust . find (isOpenAndIsolated . activeAsgDesc)

    isolatedAssignments = filter (isOpenAndIsolated . activeAsgDesc)

    groupRegistration = Pages.groupRegistration ()

    headerLine msg isLimited = tr $ do
      th ""
      th (fromString $ msg $ msg_Home_Course "Course")
      th (fromString $ msg $ msg_Home_CourseAdmin "Teacher")
      th (fromString $ msg $ msg_Home_Assignment "Assignment")
      when isLimited $ th (fromString $ msg $ msg_Home_Limit "Limit")
      th (fromString $ msg $ msg_Home_Deadline "Deadline")
      th (fromString $ msg $ msg_Home_Evaluation "Evaluation")

    assignmentLine msg isLimited (k,a,s) = H.tr $ do
      case and [aActive a, noLimitIsReached a] of
        True -> td $ Content.link (routeWithParams (Pages.submission ()) [requestParam k]) (msg $ msg_Home_NewSolution "New submission")
        False -> td (fromString . msg $ msg_Home_ClosedSubmission "Closed")
      td (fromString . aGroup $ a)
      td (fromString . join . intersperse ", " . aTeachers $ a)
      td $ linkWithText (routeWithParams (Pages.submissionList ()) [requestParam k]) (fromString (aTitle a))
      when isLimited $ td (fromString . limit $ aLimit a)
      td (fromString . showDate . timeconverter $ aEndDate a)
      let grayLabel  tag = H.span ! class_ "label label-default" $ tag
      let greenLabel tag = H.span ! class_ "label label-success" $ tag
      let redLabel   tag = H.span ! class_ "label label-danger"  $ tag
      let blueLabel  tooltip tag = H.span ! class_ "label label-primary" ! A.title (fromString tooltip) $ tag
      H.td $ withSubmissionInfo s
               (grayLabel $ fromString $ msg $ msg_Home_SubmissionCell_NoSubmission "No submission")
               (grayLabel $ fromString $ msg $ msg_Home_SubmissionCell_NonEvaluated "Non-evaluated")
               (bool (grayLabel $ fromString $ msg $ msg_Home_SubmissionCell_Tests_Passed "Tests are passed")
                     (grayLabel $ fromString $ msg $ msg_Home_SubmissionCell_Tests_Failed "Tests are failed"))
               (\_key result -> evResult
                                  (greenLabel $ fromString $ msg $ msg_Home_SubmissionCell_Accepted "Accepted")
                                  (redLabel   $ fromString $ msg $ msg_Home_SubmissionCell_Rejected "Rejected")
                                  (blueLabel "" . fromString)
                                  (\resultText -> let cell = if length resultText < displayableFreeFormResultLength
                                                        then resultText
                                                        else msg $ msg_Home_SubmissionCell_FreeFormEvaluated "Evaluated"
                                                  in blueLabel resultText $ fromString cell)
                                  result)
      where
        noLimitIsReached = submissionLimit (const True) (\n _ -> n > 0) (const False) . aLimit
        limit = fromString . submissionLimit
          (const "") (\n _ -> (msg $ msg_Home_Remains "Remains: ") ++ show n) (const $ msg $ msg_Home_Reached "Reached")

        evResult passed failed percentage freeFormMsg =
          evResultCata
            (binaryCata (resultCata passed failed))
            score
            (freeForm freeFormMsg)
          where
            percent x = join [show . round $ (100 * x), "%"]
            score (Percentage (Scores [p])) = percentage $ percent p
            score _                         = error "SubmissionTable.coloredSubmissionCell percentage is not defined"

availableAssessment :: I18N -> (Course, [(AssessmentKey, ScoreInfo)]) -> Html
availableAssessment msg (c, assessments) =
  Bootstrap.rowColMd12 . Bootstrap.table $ do
    H.tr (header assessments)
    H.tr $ do
      H.td . string $ courseName c
      mapM_ evaluationViewButton (zip assessments [1..])
  where
      header assessments = H.th mempty >> mapM_ (H.td . assessmentButton) (take (length assessments) [1..])
          where
            assessmentButton :: Int -> Html
            assessmentButton n = Bootstrap.buttonLink "" ("A" ++ show n)
        
      evaluationViewButton :: ((AssessmentKey,ScoreInfo),Int) -> Html
      evaluationViewButton ((ak,info),n) = H.td $ scoreInfoToIcon msg "/home" "/home" info
