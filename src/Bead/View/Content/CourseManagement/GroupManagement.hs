{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.CourseManagement.GroupManagement (
    courseAdminPage
  , createGroup
  , assignGroupAdmin
  ) where

import           Control.Monad
import           Control.Arrow ((***), (&&&))
import           Data.Function (on)
import           Data.List (intercalate, sortBy)
import           Data.String (fromString)
import           Data.Tuple.Utils (snd3)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content hiding (table, option)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.RequestParams (courseKeyParamName)
import qualified Bead.View.UserActions as UA (UserAction(..))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type Admin = User

data PageData = PageData {
    pdCourseKey   :: CourseKey
  , pdCourse      :: Course
  , pdGroups      :: [(GroupKey, Group, [Admin])]
  , pdGroupAdmins :: [Admin]
  }

courseAdminPage :: ContentHandler IHtml
courseAdminPage = do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  pageData <- userStory $ do
    Story.isAdministratedCourse ck
    (course, gkeys) <- Story.loadCourse ck
    groups <- forM gkeys $ \gk -> do
      group_ <- Story.loadGroup gk
      admins <- Story.groupAdmins gk
      return (gk, group_, admins)
    admins <- Story.allAdministrators
    return PageData {
        pdCourseKey   = ck
      , pdCourse      = course
      , pdGroups      = groups
      , pdGroupAdmins = admins
      }
  return $ courseAdminContent pageData

-- * Create group

createGroup :: ModifyHandler
createGroup = ModifyHandler $ do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  g <- getGroup
  return $ Action $ do
    Story.createGroup ck g
    return $ redirection $ Pages.courseManagement ck Pages.GroupManagementContents ()

-- * Assign GroupAdmin to a group

assignGroupAdmin :: ModifyHandler
assignGroupAdmin = ModifyHandler submitGroupAdmin

submitGroupAdmin :: POSTContentHandler
submitGroupAdmin = do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  admin <- getParameter (jsonUsernamePrm (fieldName selectedGroupAdmin))
  group_ <- getParameter (jsonGroupKeyPrm (fieldName selectedGroup))
  return $ Action $ do
    Story.createGroupAdmin admin group_
    return $ redirection $ Pages.courseManagement ck Pages.GroupManagementContents ()

-- * View

groupAdministratorsTable :: I18N -> [(GroupKey, Group, [Admin])] -> H.Html
groupAdministratorsTable _ [] = return ()
groupAdministratorsTable i18n groups = do
  let groups' = sortBy (compareHun `on` (groupName . snd3)) groups
  Bootstrap.row $ Bootstrap.colMd12  $ Bootstrap.table $ do
    H.thead $ H.tr $ do
      H.th (fromString . i18n $ msg_CourseAdmin_GroupAdmins_Group "Group")
      H.th (fromString . i18n $ msg_CourseAdmin_GroupAdmins_Admins "Group Admins")
    H.tbody $ forM_ groups' $ \(gk, group_, admins) -> do
      H.tr $ do
        H.td (fromString $ groupName group_)
        H.td (fromString . intercalate ", " $ Prelude.map u_name admins)

courseAdminContent :: PageData -> IHtml
courseAdminContent info = do
  msg <- getI18N
  return $ do
    Bootstrap.row $ do
      -- New Group for the course
      Bootstrap.colMd12 $ do
        H.h3 $ (fromString $ msg $ msg_CourseAdmin_CreateGroup "New group for the course")
        postForm (routeOf createGroup) $ do
          Bootstrap.textInput (fieldName groupNameField) (msg $ msg_Input_Group_Name "Title") ""
          Bootstrap.textInput (fieldName groupDescField) (msg $ msg_Input_Group_Description "Description") ""
          Bootstrap.submitButton (fieldName createGroupBtn) (fromString $ msg $ msg_CourseAdmin_CreateCourse "Create group")
          H.hr
      -- Assign teacher to the group
      Bootstrap.colMd12 $ do
        H.h3 $ (fromString $ msg $ msg_CourseAdmin_AssignAdmin "Assign teacher to the group")
        nonEmpty groups (H.p $ fromString . msg $ msg_CourseAdmin_NoGroups "There are no groups.") $
          nonEmpty groupAdmins (H.p $ fromString . msg $ msg_CourseAdmin_NoGroupAdmins "There are no teachers.") $
          postForm (routeOf assignGroupAdmin) $ do
            Bootstrap.selection (fieldName selectedGroup) (const False) groups
            Bootstrap.selection (fieldName selectedGroupAdmin) (const False) groupAdmins
            Bootstrap.submitButton (fieldName assignGroupAdminBtn) (fromString $ msg $ msg_CourseAdmin_AssignAdmin_Button "Assign")

    when (not . null $ groups) $ do
      Bootstrap.row $ Bootstrap.colMd12 $ H.hr
      Bootstrap.row $ Bootstrap.colMd12 $ H.p $
        fromString $ msg $ msg_CourseAdmin_GroupAdmins_Info
          "The following table contains the course related groups and group admins."

    -- Group Administrators table
    groupAdministratorsTable msg (pdGroups info)

  where
    groups :: [(GroupKey, String)]
    groups = map (\(gk, grp, admins) -> (gk, shortGroupName grp)) (pdGroups info)

    groupAdmins :: [(Username, String)]
    groupAdmins = Prelude.map (u_username &&& userLongname) (pdGroupAdmins info)

    userLongname :: User -> String
    userLongname u = concat [ u_name u, " - ", usernameCata Prelude.id $ u_username u ]

    createGroup = Pages.createGroup (pdCourseKey info) ()
    assignGroupAdmin = Pages.assignGroupAdmin (pdCourseKey info) ()

getGroup = Group
  <$> getParameter (stringParameter (fieldName groupNameField) "Group name")
  <*> getParameter (stringParameter (fieldName groupDescField) "Group description")
