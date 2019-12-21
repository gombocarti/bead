{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.SubmissionTable (
    AdministratedCourses
  , AdministratedGroups
  , CourseTestScriptInfos
  , SubmissionTableContext(..)
  , submissionTable
  , submissionTableContext
  , sortUserLines
  , groupButtonStyle
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
import           Bead.View.Content.VisualConstants
import           Bead.View.Content.SubmissionState (formatSubmissionState, toLargeIcon)
import qualified Bead.View.DataBridge as Param

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type AdministratedCourses = Map CourseKey E.Course

type AdministratedGroups  = Map GroupKey  (E.Group, String)

type CourseTestScriptInfos = Map CourseKey [(TestScriptKey, TestScriptInfo)]

data SubmissionTableContext = SubmissionTableContext {
    stcUsername     :: Username
  , stcAdminCourses :: AdministratedCourses
  , stcAdminGroups  :: AdministratedGroups
  , stcCourseTestScriptInfos :: CourseTestScriptInfos
  }

submissionTableContextCata f (SubmissionTableContext username courses groups testscripts)
  = f username courses groups testscripts

submissionTableContext :: UserStory SubmissionTableContext
submissionTableContext = do
  u <- S.username
  ac <- S.administratedCourses
  ag <- S.administratedGroups
  ts <- Map.fromList <$> mapM (testScriptForCourse . fst) ac
  return $! SubmissionTableContext {
      stcUsername     = u
    , stcAdminCourses = adminCourseMap ac
    , stcAdminGroups  = adminGroupMap ag
    , stcCourseTestScriptInfos = ts
    }
  where
    testScriptForCourse ck = do
      infos <- S.testScriptInfos ck
      return (ck, infos)

    adminCourseMap = Map.fromList

    adminGroupMap = Map.fromList . map (\(k,g,c) -> (k,(g,c)))

submissionTable :: String -> UTCTime -> SubmissionTableContext -> SubmissionTableInfo -> IHtml
submissionTable tableId now stb table = submissionTableContextCata html stb where
  html _username courses groups testscripts = do
    msg <- getI18N
    return $ do
      H.h4 . H.b $ fromString $ unwords [stiCourse table, userCountText msg]
      i18n msg $ managementMenu courses groups table
      i18n msg $ submissionTablePart tableId now stb table
      i18n msg $ courseTestScriptTable testscripts table

  userCountText msg = concat ["(", show userCount, " "
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
        H.td (fromString $ msg $ msg_Home_SubmissionTable_NoCoursesOrStudents "There are no assignments or students yet.")

-- Non empty table
submissionTablePart tableId now ctx s = do
  msg <- getI18N
  return $ do
    courseForm $ Bootstrap.rowColMd12 $ do
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
      course _n _us _as _uls _grps ck = postForm (routeOf $ Pages.deleteUsersFromCourse ck ())
      group _n _us _cgas _uls _ck gk  = postForm (routeOf $ Pages.deleteUsersFromGroup gk ())

    headerCell = H.th

    assignmentLine msg = H.tr $ do
      headerCell $ fromString $ msg $ msg_Home_SubmissionTable_StudentName "Name"
      headerCell $ fromString $ msg $ msg_Home_SubmissionTable_Username "Username"
      assignmentLinks
      groupAndAdminHeader
      deleteHeaderCell msg
      where
        groupAndAdminHeader :: H.Html
        groupAndAdminHeader = submissionTableInfoCata course group s
          where
            course _name _users _as _ulines _grps _key = do
              headerCell $ fromString $ msg $ msg_Home_SubmissionTable_Group "Group"
              headerCell $ fromString $ msg $ msg_Home_SubmissionTable_Admins "Admins"

            group  _name _users _cgas _ulines _ckey _gkey = mempty

        assignmentLinks = submissionTableInfoCata course group s

        course _name _users as ulines _grps _key =
          mapM_ (\(i, info@(ak, a, hasTestCase)) ->
                   let exportAdminedGroups = Pages.exportSubmissionsOfGroups ak (stcUsername ctx) ()
                       exportAll = Pages.exportSubmissions ak ()
                       exportLinks = map (enableIf [hasSolutions]) [exportAdminedGroups, exportAll]
                       hasSolutions = if hasSubmissions ak (map snd ulines)
                                      then Nothing
                                      else Just (msg $ msg_AssignmentDoesntHaveSubmissions "The assignment does not have submissions.")
                       hasTest = case hasTestCase of
                                   HasTestCase -> Nothing
                                   DoesNotHaveTestCase -> Just (msg $ msg_AssignmentDoesntHaveTestCase "The assignment does not have test case.")
                       queueAllSubmissions = enableIf [hasTest, hasSolutions] (Pages.queueAllSubmissionsForTest ak ())
                       dropdownItems =  queueAllSubmissions : Bootstrap.Separator : exportLinks
                   in modifyAssignmentLink msg courseButtonStyle (i, info) dropdownItems)
                (zip [1..] as)

        group  _name _users cgas ulines ckey gkey = mapM_ header (zip [1..] cgas)
          where
            header :: (Int, CGInfo (AssignmentKey, Assignment, HasTestCase)) -> H.Html
            header (i, cga) =
              let info@(ak, a, hasTestCase) = cgInfoCata id id cga
                  exportOneGroup = Pages.exportSubmissionsOfOneGroup ak gkey ()
                  exportAdminedGroups = Pages.exportSubmissionsOfGroups ak (stcUsername ctx) ()
                  exportAll = Pages.exportSubmissions ak ()
                  hasSolutions = if hasSubmissions ak (map snd ulines)
                                 then Nothing
                                 else Just (msg $ msg_AssignmentDoesntHaveSubmissions "The assignment does not have submissions.")
                  hasTest = case hasTestCase of
                              HasTestCase -> Nothing
                              DoesNotHaveTestCase -> Just (msg $ msg_AssignmentDoesntHaveTestCase "The assignment does not have test case.")
                  queueAllSubmissions = enableIf [hasTest, hasSolutions] (Pages.queueAllSubmissionsForTest ak ())
             in cgInfoCata
                  (\_ -> -- course assignment
                     if courseIsAdmined
                     then let exportLinks = map (enableIf [hasSolutions]) [exportOneGroup, exportAdminedGroups, exportAll]
                              dropdownItems =  queueAllSubmissions : Bootstrap.Separator : exportLinks
                          in modifyAssignmentLink msg courseButtonStyle (i, info) dropdownItems
                     else let exportLinks = map (enableIf [hasSolutions]) [exportOneGroup, exportAdminedGroups]
                              dropdownItems = queueAllSubmissions : Bootstrap.Separator : exportLinks
                          in viewAssignmentLink msg courseButtonStyle ckey (i, info) dropdownItems)
                  (\_ -> -- group assignment
                     let dropdownItems = [queueAllSubmissions, Bootstrap.Separator, enableIf [hasSolutions] exportAll]
                     in modifyAssignmentLink msg groupButtonStyle (i, info) dropdownItems)
                  cga

            courseIsAdmined :: Bool
            courseIsAdmined = isAdminedCourse ckey

        enable :: Pages.PageDesc -> MenuItem
        enable p = Enabled (routeOf p) (msg $ linkText p)

        disable :: String -> Pages.PageDesc -> MenuItem
        disable reason p = Disabled (msg $ linkText p) reason

        enableIf :: [Maybe String] -> Pages.PageDesc -> MenuItem
        enableIf reasonsToDisable = case catMaybes reasonsToDisable of
                                      [] -> enable
                                      (reason:_) -> disable reason

        hasSubmissions :: AssignmentKey -> [Map AssignmentKey (SubmissionKey, b)] -> Bool
        hasSubmissions ak = any (isJust . Map.lookup ak)

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

    viewAssignmentLink :: I18N -> (String, String) -> CourseKey -> (Int, (AssignmentKey, Assignment, HasTestCase)) -> [MenuItem] -> H.Html
    viewAssignmentLink msg _buttonStyle@(active, passive) ck (i,(ak,a,_)) dropdownItems =
      H.td $ Bootstrap.customSplitButton
               [if (Assignment.isActive a now) then active else passive]
               (routeOf $ Pages.viewAssignment ak ())
               (Assignment.name a)
               (show i)
               dropdownItems

    isAdminedCourse :: CourseKey -> Bool
    isAdminedCourse ck = Map.member ck (stcAdminCourses ctx)

    userLine :: I18N
             -> SubmissionTableInfo
             -> (UserDesc, Map AssignmentKey (SubmissionKey, SubmissionState))
             -> H.Html
    userLine msg s (u, submissionMap) = do
      H.tr $ do
        let username = ud_username u
        H.td . fromString $ ud_fullname u
        H.td . fromString $ uid id $ ud_uid u
        submissionCells msg username s
        groupInfo username s
        deleteUserCheckbox u
      where
        submissionCells :: I18N -> Username -> SubmissionTableInfo -> H.Html
        submissionCells msg username = submissionTableInfoCata course group where
          course _n _users as _ulines _grps _key = mapM_ (submissionInfoCell msg username) as

          group _n _users as _ulines _ck _gk =
            mapM_ (cgInfoCata (submissionInfoCell msg username) (submissionInfoCell msg username)) as

        submissionInfoCell :: I18N -> Username -> (AssignmentKey, Assignment, HasTestCase) -> H.Html
        submissionInfoCell msg u (ak, _, _) = case Map.lookup ak submissionMap of
          Nothing -> H.td mempty
          Just ss -> submissionCell msg u ss

        groupInfo :: Username -> SubmissionTableInfo -> H.Html
        groupInfo u = submissionTableInfoCata course group where
          course _n _users _as _ulines grps _key =
            case Map.lookup u grps of
              Nothing -> H.td mempty <> H.td mempty
              Just (grp, admins) -> H.td (fromString $ groupName grp) <> H.td (fromString $ intercalate "," $ map u_name admins)

          group _n _users _as _ulines _ck _gk = mempty

    submissionCell :: I18N -> Username -> (SubmissionKey, SubmissionState) -> H.Html
    submissionCell msg u (sKey, sState) =
      H.td . Bootstrap.link route $ formatSubmissionState toLargeIcon msg sState

      where
        route :: String
        route = routeOf $ case siEvaluationKey sState of
                            Nothing -> Pages.evaluation sKey ()
                            Just ek -> Pages.modifyEvaluation sKey ek ()

    deleteHeaderCell msg = submissionTableInfoCata deleteForCourseButton deleteForGroupButton s where
        deleteForCourseButton _n _us _as _uls _grps _ck =
          headerCell $ submitButtonDanger
            removeButton
            (msg $ msg_Home_DeleteUsersFromCourse "Remove") ! A.disabled ""

        deleteForGroupButton _n _us _as _uls _ck _gk =
          headerCell $ submitButtonDanger
            removeButton
            (msg $ msg_Home_DeleteUsersFromGroup "Remove") ! A.disabled ""

    deleteUserCheckbox u = submissionTableInfoCata deleteCourseCheckbox deleteGroupCheckbox s where
        deleteCourseCheckbox _n _us _as _uls _grps _ck =
          H.td $ checkBox
            (Param.name delUserFromCoursePrm)
            (encode delUserFromCoursePrm $ ud_username u)
            False ! A.onclick (fromString (onClick ++ "(this)"))

        deleteGroupCheckbox _n _us _as _uls _ck _gk =
          H.td $ checkBox
            (Param.name delUserFromGroupPrm)
            (encode delUserFromGroupPrm $ ud_username u)
            False ! A.onclick (fromString (onClick ++ "(this)"))


courseTestScriptTable :: CourseTestScriptInfos -> SubmissionTableInfo -> IHtml
courseTestScriptTable cti = submissionTableInfoCata course group where
  course _n _us _as _uls _grps ck = testScriptTable cti ck
  group _n _us _as _uls _ck _gk = (return (return ()))

-- Renders a course test script modification table if the information is found in the
-- for the course, otherwise an error message. If the course is found, and there is no
-- test script found for the course a message indicating that will be rendered, otherwise
-- the modification table is rendered
testScriptTable :: CourseTestScriptInfos -> CourseKey -> IHtml
testScriptTable cti ck = maybe (return "") courseFound $ Map.lookup ck cti where
  courseFound ts = do
    msg <- getI18N
    return $ do
      Bootstrap.rowColMd12 $ do
        H.h3 $ fromString $ msg $ msg_Home_ModifyTestScriptTable "Testers"
        case ts of
          []  -> H.p $ fromString $ msg $ msg_Home_NoTestScriptsWereDefined "There are no testers for the course."
          ts' -> Bootstrap.unorderedListGroup $ forM_ ts' $ \(tsk, tsi) ->
                   Bootstrap.listGroupLinkItem
                     (routeOf (Pages.modifyTestScript tsk ()))
                     (fromString $ tsiName tsi)

-- Renders a menu for the creation of the course or group assignment and evaluation export
-- if the user administrates the given group or course
managementMenu
  :: AdministratedCourses
  -> AdministratedGroups
  -> SubmissionTableInfo
  -> IHtml
managementMenu courses groups = submissionTableInfoCata courseMenu groupMenu
  where
    groupMenu _n _us _as _uls ck gk = maybe
      (return (return ()))
      (const $ do
        msg <- getI18N
        return . navigationWithRoute $
          case Map.lookup ck courses of
            Nothing -> map (button msg) [Pages.newGroupAssignment gk (), Pages.newGroupAssessment gk (), Pages.exportEvaluationsScoresAdminedGroups ck ()]
            Just _  -> map (button msg) [Pages.newGroupAssignment gk (), Pages.newCourseAssignment ck (), Pages.newGroupAssessment gk ()]
                         ++ [dropdown msg [Pages.exportEvaluationsScoresAdminedGroups ck (), Pages.exportEvaluationsScoresAllGroups ck ()]])
      (Map.lookup gk groups)

    courseMenu _n _us _as _uls _grps ck = maybe
      (return (return ()))
      (const $ do
        msg <- getI18N
        return (navigationWithRoute $
                  [button msg $ Pages.newCourseAssignment ck ()]
                   ++ [dropdown msg [Pages.exportEvaluationsScoresAdminedGroups ck (), Pages.exportEvaluationsScoresAllGroups ck ()]]))
      (Map.lookup ck courses)

    navigationWithRoute :: [H.Html] -> H.Html
    navigationWithRoute buttons =
      Bootstrap.rowColMd12 . Bootstrap.buttonGroup $ mconcat buttons

    button :: I18N -> Pages.PageDesc -> H.Html
    button msg page = Bootstrap.buttonLink (routeOf page) (msg $ linkText page)

    dropdown :: I18N -> [Pages.PageDesc] -> H.Html
    dropdown msg pages = Bootstrap.dropdown
      (msg $ msg_Home_SubmissionTable_ExportEvaluations "Export Evaluations")
      (map (\page -> (Enabled (routeOf page) (msg $ linkText page))) pages)

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
  course name users assignments userlines groups key =
      CourseSubmissionTableInfo name users assignments (sort userlines) groups key

  group name users assignments userlines ckey gkey =
      GroupSubmissionTableInfo name users assignments (sort userlines) ckey gkey

  sort :: [(UserDesc, a)] -> [(UserDesc, a)]
  sort = sortOn fst

submissionTableInfoAssignments = submissionTableInfoCata course group where
  course _n _us as _uls _grps _ck = as
  group _n _us cgas _uls _ck _gk = map (cgInfoCata id id) cgas

headLine = H.tr . H.th . fromString
