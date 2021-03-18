{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.CourseManagement.Page (
    courseManagement
  ) where

import           Control.Monad.IO.Class
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H

import qualified Bead.Controller.UserStories as Story
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Domain.Relationships as R
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.CourseManagement.GroupManagement (courseAdminPage)
import           Bead.View.Content.CourseManagement.TestScript (newTestScriptPage, modifyTestScriptPage)
import           Bead.View.Content.CourseManagement.TestScripts (testScriptsPage)
import           Bead.View.Content.SubmissionTable
import           Bead.View.ContentHandler (setHomePage)
import           Bead.View.RequestParams

courseManagement :: ViewHandler
courseManagement = ViewHandler $ do
  contentsParam <- getParameter courseManagementContentsPrm
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  course <- fmap fst . userStory $ Story.loadCourse ck
  msg <- i18nE
  body <- Pages.courseManagementContentsCata
    courseAdminPage
    courseSubmissionsPage
    testScriptsPage
    newTestScriptPage
    modifyTestScriptPage
    contentsParam
  setPageContents $ HtmlPage {
      pageTitle = return $ Bootstrap.pageHeader (courseName course) Nothing
    , pageBody = fmap (navigationTabs msg contentsParam ck <>) body
    }

courseSubmissionsPage :: ContentHandler IHtml
courseSubmissionsPage = do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  now <- liftIO $ getCurrentTime
  (stc,sti) <- userStory $ do
    stc <- submissionTableContext
    sti <- sortUserLines <$> Story.courseSubmissionTable ck
    return (stc, sti)
  setHomePage $ R.CourseManagement ck
  return $ courseSubmissionsContent now ck stc sti

navigationTabs :: I18N -> Pages.CourseManagementContents -> CourseKey -> H.Html
navigationTabs msg contents ck = Bootstrap.tab $ mconcat $ map (tabToHtml msg) tabs
  where
    tabs :: [(Pages.CourseManagementContents, Translation String)]
    tabs = [ (Pages.GroupManagementContents, msg_LinkText_GroupManagement "Group Management")
           , (Pages.TestScriptsContents, msg_LinkText_TestScripts "Test Scripts")
           , (Pages.AssignmentsContents, msg_LinkText_CourseOverview "Course Overview")
           ]

    tabToHtml :: I18N -> (Pages.CourseManagementContents, Translation String) -> H.Html
    tabToHtml msg (tab, text) = toItem (routeOf page) (T.pack . msg $ text)

      where
        page :: Pages.PageDesc
        page = Pages.courseManagement ck tab ()

        toItem :: String -> Text -> H.Html
        toItem
          | tab == (activeTabOf contents) = Bootstrap.tabItemActive
          | otherwise = Bootstrap.tabItem

        activeTabOf :: Pages.CourseManagementContents -> Pages.CourseManagementContents
        activeTabOf = Pages.courseManagementContentsCata
                        Pages.GroupManagementContents     -- GroupManagementContents
                        Pages.AssignmentsContents         -- AssignmentContents
                        Pages.TestScriptsContents         -- TestScriptsContents
                        Pages.TestScriptsContents         -- NewTestScriptContents
                        (const Pages.TestScriptsContents) -- ModifyTestScriptContents

courseSubmissionsContent :: UTCTime -> CourseKey -> SubmissionTableContext -> SubmissionTableInfo -> IHtml
courseSubmissionsContent now ck c s = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $
      H.p $ fromString . msg $ msg_Home_SubmissionTable_Info $ concat
        [ "Assignments may be modified by clicking on their identifiers if you have rights for the modification (their names are shown in the tooltip).  "
        , "Students may be unregistered from the courses or the groups by checking the boxes in the Remove column "
        , "then clicking on the button."
        ]
    i18n msg $ submissionTable "course-table" now c s
