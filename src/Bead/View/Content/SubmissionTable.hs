{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.SubmissionTable (
    AdministratedCourses
  , SubmissionTableContext(..)
  , submissionTable
  , submissionTableContext
  , sortUserLines
  , groupButtonStyle
  , deleteUsersFromCourse
  , deleteUsersFromGroup
  ) where

import           Control.Monad
import           Data.Char (isAlphaNum)
import           Data.Function
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Time
import           Data.Text (Text)
import qualified Data.Text as T
import           Numeric

import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.Evaluation
import           Bead.Domain.Relationships
import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (UserStory)
import qualified Bead.Controller.UserStories as S
import           Bead.View.Content
import           Bead.View.Content.Bootstrap (MenuItem(Enabled, Disabled))
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.StateVisualization (formatSubmissionState, toLargeIcon)
import           Bead.View.Content.VisualConstants
import qualified Bead.View.DataBridge as Param

import qualified Text.Blaze as B
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

deleteUsersFromCourse :: ModifyHandler
deleteUsersFromCourse = ModifyHandler deleteUsersFromCourseHandler

deleteUsersFromGroup :: ModifyHandler
deleteUsersFromGroup = ModifyHandler deleteUsersFromGroupHandler

type AdministratedCourses = [CourseKey]

data SubmissionTableContext = SubmissionTableContext {
    stcUsername     :: Username
  , stcAdminCourses :: AdministratedCourses
  }

submissionTableContextCata f (SubmissionTableContext username courses)
  = f username courses

submissionTableContext :: UserStory SubmissionTableContext
submissionTableContext = do
  u <- S.username
  ac <- S.administratedCourses
  return $! SubmissionTableContext {
      stcUsername     = u
    , stcAdminCourses = map fst ac
    }

submissionTable :: String -> UTCTime -> SubmissionTableContext -> SubmissionTableInfo -> IHtml
submissionTable tableId now stb table = submissionTableContextCata html stb where
  html _username courses = do
    msg <- getI18N
    return $ Bootstrap.rowColMd12 $ do
      i18n msg $ managementMenu courses table
      B.toMarkup $ userCountText msg
      i18n msg $ submissionTablePart tableId now stb table

  userCountText msg = T.concat
    ["(", T.pack $ show userCount, " "
    , if (userCount == 1)
      then msg $ msg_Home_SubmissionTable_Student "student"
      else msg $ msg_Home_SubmissionTable_Students "students"
    , ")"
    ]
  userCount = length (stiUsers table)

-- Produces the HTML table from the submission table information,
-- if there is no users registered and submission posted to the
-- group or course students, an informational text is shown.
-- Supposing that the given tableid is unique name on the page.
submissionTablePart :: String -> UTCTime -> SubmissionTableContext -> SubmissionTableInfo -> IHtml

-- Empty table
submissionTablePart _tableId _now _ctx s
  | and [null $ submissionTableInfoAssignments s, null $ stiUsers s] = do
    msg <- getI18N
    return $ do
      Bootstrap.rowColMd12 $ Bootstrap.table $ do
        H.td (B.toMarkup $ msg $ msg_Home_SubmissionTable_NoCoursesOrStudents "There are no assignments or students yet.")

-- Non empty table
submissionTablePart tableId now ctx s = do
  msg <- getI18N
  return $ do
    courseForm $ do
      Bootstrap.table $ do
        checkedUserScript
        assignmentLine msg
        mapM_ (userLine msg s) (stiUserLines s)
  where
    -- JavaScript
    tableIdJSName = filter isAlphaNum tableId
    noOfUsers = tableIdJSName ++ "NoOfUsers"
    onCheck = tableIdJSName ++ "OnCheck"
    onUncheck = tableIdJSName ++ "OnUncheck"
    removeButton = tableIdJSName ++ "Button"
    onClick = tableIdJSName ++ "OnClick"
    checkedUserScript = H.script $ fromString $ unlines
      [ concat ["var ", noOfUsers, " = 0;"]
      , concat ["function ", onCheck, "(){"]
      ,   noOfUsers ++ "++;"
      ,   concat ["if(", noOfUsers, " > 0) {"]
      ,     concat ["document.getElementById(\"",removeButton,"\").disabled = false;"]
      ,   "}"
      , "}"
      , concat ["function ", onUncheck, "(){"]
      ,   noOfUsers ++ "--;"
      ,   concat ["if(", noOfUsers, " < 1) {"]
      ,     concat ["document.getElementById(\"",removeButton,"\").disabled = true;"]
      ,     noOfUsers ++ " = 0;"
      ,   "}"
      , "}"
      , concat ["function ", onClick, "(checkbox){"]
      ,   "if(checkbox.checked) {"
      ,      onCheck ++ "();"
      ,   "} else {"
      ,      onUncheck ++ "();"
      ,   "}"
      , "}"
      ]

    -- HTML
    courseForm = submissionTableInfoCata course group s where
      course _us _as _uls _grps ck = postForm (routeOf $ Pages.deleteUsersFromCourse ck ())
      group _us _cgas _uls _ck gk  = postForm (routeOf $ Pages.deleteUsersFromGroup gk ())

    headerCell = H.th

    assignmentLine msg = H.tr $ do
      headerCell $ B.toMarkup $ msg $ msg_Home_SubmissionTable_StudentName "Name"
      headerCell $ B.toMarkup $ msg $ msg_Home_SubmissionTable_Username "Username"
      assignmentLinks
      groupAndAdminHeader
      deleteHeaderCell msg
      where
        groupAndAdminHeader :: H.Html
        groupAndAdminHeader = submissionTableInfoCata course group s
          where
            course _users _as _ulines _grps _key = do
              headerCell $ B.toMarkup $ msg $ msg_Home_SubmissionTable_Group "Group"
              headerCell $ B.toMarkup $ msg $ msg_Home_SubmissionTable_Admins "Admins"

            group _users _cgas _ulines _ckey _gkey = mempty

        assignmentLinks = submissionTableInfoCata course group s

        course _users as ulines _grps ckey =
          mapM_ (\(i, info@(ak, a, hasTestCase)) ->
                   let exportAdminedGroups = Pages.exportSubmissionsOfGroupsWithText ak (stcUsername ctx)
                       exportAll = Pages.exportSubmissionsWithText ak
                       exportLinks = map (enableIf [hasSolutions]) [exportAdminedGroups, exportAll]
                       hasSolutions = if hasSubmissions i (map snd ulines)
                                      then Nothing
                                      else Just (msg $ msg_AssignmentDoesntHaveSubmissions "The assignment does not have submissions.")
                       hasTest = case hasTestCase of
                                   HasTestCase -> Nothing
                                   DoesNotHaveTestCase -> Just (msg $ msg_AssignmentDoesntHaveTestCase "The assignment does not have test case.")
                       similarityCheck = enableIf [hasSolutions] (Pages.similarityCheckMossWithText ak)
                       queueAllSubmissions = enableIf [hasTest, hasSolutions] (Pages.queueAllSubmissionsForTestWithText ak)
                       dropdownItems = queueAllSubmissions : Bootstrap.Separator : similarityCheck : Bootstrap.Separator : exportLinks
                   in if courseIsAdmined
                      then modifyAssignmentLink msg courseButtonStyle (i, info) dropdownItems
                      else viewAssignmentLink msg courseButtonStyle (i, info) dropdownItems)
                (zip [1..] as)

          where
            courseIsAdmined :: Bool
            courseIsAdmined = isAdminedCourse ckey

        group _users cgas ulines ckey gkey = mapM_ header (zip [1..] cgas)
          where
            header :: (Int, CGInfo (AssignmentKey, Assignment, HasTestCase)) -> H.Html
            header (i, cga) =
              let info@(ak, a, hasTestCase) = cgInfoCata id id cga
                  exportOneGroup = Pages.exportSubmissionsOfOneGroupWithText ak gkey
                  exportAdminedGroups = Pages.exportSubmissionsOfGroupsWithText ak (stcUsername ctx)
                  exportAll = Pages.exportSubmissionsWithText ak
                  hasSolutions = if hasSubmissions i (map snd ulines)
                                 then Nothing
                                 else Just (msg $ msg_AssignmentDoesntHaveSubmissions "The assignment does not have submissions.")
                  hasTest = case hasTestCase of
                              HasTestCase -> Nothing
                              DoesNotHaveTestCase -> Just (msg $ msg_AssignmentDoesntHaveTestCase "The assignment does not have test case.")
                  similarityCheck = enableIf [hasSolutions] (Pages.similarityCheckMossWithText ak)
                  queueAllSubmissions = enableIf [hasTest, hasSolutions] (Pages.queueAllSubmissionsForTestWithText ak)
             in cgInfoCata
                  (\_ -> -- course assignment
                     if courseIsAdmined
                     then let exportLinks = map (enableIf [hasSolutions]) [exportOneGroup, exportAdminedGroups, exportAll]
                              dropdownItems = queueAllSubmissions : Bootstrap.Separator : similarityCheck : Bootstrap.Separator : exportLinks
                          in modifyAssignmentLink msg courseButtonStyle (i, info) dropdownItems
                     else let exportLinks = map (enableIf [hasSolutions]) [exportOneGroup, exportAdminedGroups]
                              dropdownItems = queueAllSubmissions : Bootstrap.Separator : similarityCheck : Bootstrap.Separator : exportLinks
                          in viewAssignmentLink msg courseButtonStyle (i, info) dropdownItems)
                  (\_ -> -- group assignment
                     let dropdownItems = [queueAllSubmissions, Bootstrap.Separator, similarityCheck, Bootstrap.Separator, enableIf [hasSolutions] exportAll]
                     in modifyAssignmentLink msg groupButtonStyle (i, info) dropdownItems)
                  cga

            courseIsAdmined :: Bool
            courseIsAdmined = isAdminedCourse ckey

        enable :: Pages.Page' Translation -> MenuItem
        enable p = Enabled (routeOf p) (msg $ Pages.pageValue p)

        disable :: Text -> Pages.Page' Translation -> MenuItem
        disable reason p = Disabled (msg $ Pages.pageValue p) reason

        enableIf :: [Maybe Text] -> Pages.Page' Translation -> MenuItem
        enableIf reasonsToDisable = case catMaybes reasonsToDisable of
                                      [] -> enable
                                      (reason:_) -> disable reason

        hasSubmissions :: Int -> [[Maybe (SubmissionKey, b)]] -> Bool
        hasSubmissions n = any (isJust . (!! (n - 1)))

    modifyAssignmentLink :: I18N -> (String, String) -> (Int, (AssignmentKey, Assignment, HasTestCase)) -> [MenuItem] -> H.Html
    modifyAssignmentLink msg _buttonStyle@(active, passive) (i, (ak, a, _hasTestCase)) dropdownItems =
      -- If the assignment is active we render with active assignment button style,
      -- if not active the closed button style
      H.td $ Bootstrap.customSplitButton
               [if (Assignment.isActive a now) then active else passive]
               (routeOf $ Pages.modifyAssignment ak ())
               (Assignment.name a)
               (show i)
               dropdownItems

    viewAssignmentLink :: I18N -> (String, String) -> (Int, (AssignmentKey, Assignment, HasTestCase)) -> [MenuItem] -> H.Html
    viewAssignmentLink msg _buttonStyle@(active, passive) (i,(ak,a,_)) dropdownItems =
      H.td $ Bootstrap.customSplitButton
               [if (Assignment.isActive a now) then active else passive]
               (routeOf $ Pages.viewAssignment ak ())
               (Assignment.name a)
               (show i)
               dropdownItems

    isAdminedCourse :: CourseKey -> Bool
    isAdminedCourse ck = ck `elem` (stcAdminCourses ctx)

    userLine :: I18N
             -> SubmissionTableInfo
             -> (UserDesc, [Maybe (SubmissionKey, SubmissionState)])
             -> H.Html
    userLine msg s (u, submissions) = do
      H.tr $ do
        let username = ud_username u
        H.td . B.toMarkup $ ud_fullname u
        H.td . B.toMarkup $ uid id $ ud_uid u
        submissionCells msg username submissions
        groupInfo username s
        deleteUserCheckbox u
      where
        submissionCells :: I18N -> Username -> [Maybe (SubmissionKey, SubmissionState)] -> H.Html
        submissionCells msg username submissions = mapM_ (submissionCell msg username) submissions

        groupInfo :: Username -> SubmissionTableInfo -> H.Html
        groupInfo u = submissionTableInfoCata course group where
          course _users _as _ulines grps _key =
            case Map.lookup u grps of
              Nothing -> H.td mempty <> H.td mempty
              Just (grp, admins) -> H.td (B.toMarkup $ groupName grp) <> H.td (B.toMarkup $ intercalate "," $ map u_name admins)

          group _users _as _ulines _ck _gk = mempty

    submissionCell :: I18N -> Username -> Maybe (SubmissionKey, SubmissionState) -> H.Html
    submissionCell _   _ Nothing = H.td mempty
    submissionCell msg u (Just (sKey, sState)) =
      H.td . Bootstrap.link route $ formatSubmissionState toLargeIcon msg sState

      where
        route :: Text
        route = routeOf $ case siEvaluationKey sState of
                            Nothing -> Pages.evaluation sKey ()
                            Just ek -> Pages.modifyEvaluation sKey ek ()

    deleteHeaderCell :: I18N -> H.Html
    deleteHeaderCell msg = submissionTableInfoCata deleteForCourseButton deleteForGroupButton s where
        deleteForCourseButton _us _as _uls _grps _ck =
          headerCell $ submitButtonDanger
            removeButton
            (msg $ msg_Home_DeleteUsersFromCourse "Remove") ! A.disabled ""

        deleteForGroupButton _us _as _uls _ck _gk =
          headerCell $ submitButtonDanger
            removeButton
            (msg $ msg_Home_DeleteUsersFromGroup "Remove") ! A.disabled ""

    deleteUserCheckbox u = submissionTableInfoCata deleteCourseCheckbox deleteGroupCheckbox s where
        deleteCourseCheckbox _us _as _uls _grps _ck =
          H.td $ checkBox
            (Param.name delUserFromCoursePrm)
            (encode delUserFromCoursePrm $ ud_username u)
            False ! A.onclick (B.toValue (onClick ++ "(this)"))

        deleteGroupCheckbox _us _as _uls _ck _gk =
          H.td $ checkBox
            (Param.name delUserFromGroupPrm)
            (encode delUserFromGroupPrm $ ud_username u)
            False ! A.onclick (B.toValue (onClick ++ "(this)"))

-- Renders a menu for the creation of the course or group assignment and evaluation export
-- if the user administrates the given group or course
managementMenu
  :: AdministratedCourses
  -> SubmissionTableInfo
  -> IHtml
managementMenu courses = submissionTableInfoCata courseMenu groupMenu
  where
    groupMenu _us _as _uls ck gk = do
      msg <- getI18N
      return . navigationWithRoute $
        if ck `elem` courses
        then map (button msg) [Pages.newGroupAssignmentWithText gk, Pages.newCourseAssignmentWithText ck, Pages.newGroupAssessmentWithText gk, Pages.groupOverviewAsStudentWithText gk]
               ++ [dropdown msg [Pages.exportEvaluationsScoresAdminedGroupsWithText ck, Pages.exportEvaluationsScoresAllGroupsWithText ck]]
        else map (button msg) [Pages.newGroupAssignmentWithText gk, Pages.newGroupAssessmentWithText gk, Pages.groupOverviewAsStudentWithText gk, Pages.exportEvaluationsScoresAdminedGroupsWithText ck]

    courseMenu _us _as _uls _grps ck
      | ck `elem` courses = do
          msg <- getI18N
          return (navigationWithRoute $
                    [button msg $ Pages.newCourseAssignmentWithText ck]
                     ++ [dropdown msg [Pages.exportEvaluationsScoresAdminedGroupsWithText ck, Pages.exportEvaluationsScoresAllGroupsWithText ck]])
      | otherwise = return (return ())

    navigationWithRoute :: [H.Html] -> H.Html
    navigationWithRoute buttons = Bootstrap.buttonGroup $ mconcat buttons

    button :: I18N -> Pages.Page' Translation-> H.Html
    button msg page = Bootstrap.buttonLink (routeOf page) (msg $ Pages.pageValue page)

    dropdown :: I18N -> [Pages.Page' Translation] -> H.Html
    dropdown msg pages = Bootstrap.dropdown
      (msg $ msg_Home_SubmissionTable_ExportEvaluations "Export Evaluations")
      (map (\page -> (Enabled (routeOf page) (msg $ Pages.pageValue page))) pages)

deleteUsersFromCourseHandler :: POSTContentHandler
deleteUsersFromCourseHandler = do
  ck <- getParameter delUserFromCourseKeyPrm
  users <- getParameterValues delUserFromCoursePrm
  return $ Action $ do
    S.deleteUsersFromCourse ck users
    return $ redirection $ Pages.courseManagement ck Pages.AssignmentsContents ()

deleteUsersFromGroupHandler :: POSTContentHandler
deleteUsersFromGroupHandler = do
  gk <- getParameter delUserFromGroupKeyPrm
  users <- getParameterValues delUserFromGroupPrm
  return $ Action $ do
    S.deleteUsersFromGroup gk users
    return $ redirection $ Pages.groupOverview gk ()

-- * CSS Section

courseButtonStyle = ("btn-hcao", "btn-hcac")
groupButtonStyle  = ("btn-hgao", "btn-hgac")

-- * Colors

newtype RGB = RGB (Int, Int, Int)

pctCellColor :: Double -> RGB
pctCellColor x = RGB (round ((1 - x) * 255), round (x * 255), 0)

colorStyle :: RGB -> String
colorStyle (RGB (r,g,b)) = join ["background-color:#", hex r, hex g, hex b]
  where
    twoDigits [d] = ['0',d]
    twoDigits ds  = ds

    hex x = twoDigits (showHex x "")

-- * Tools

sortUserLines = submissionTableInfoCata course group where
  course users assignments userlines groups key =
      CourseSubmissionTableInfo users assignments (sort userlines) groups key

  group users assignments userlines ckey gkey =
      GroupSubmissionTableInfo users assignments (sort userlines) ckey gkey

  sort :: [(UserDesc, a)] -> [(UserDesc, a)]
  sort = sortOn fst

submissionTableInfoAssignments = submissionTableInfoCata course group where
  course _us as _uls _grps _ck = as
  group _us cgas _uls _ck _gk = map (cgInfoCata id id) cgas


