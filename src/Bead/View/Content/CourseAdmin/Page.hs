{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.CourseAdmin.Page (
    courseAdmin
  , createGroup
  , assignGroupAdmin
  ) where

import           Control.Monad
import           Control.Arrow ((***), (&&&))
import           Data.Function (on)
import           Data.List (intersperse, sortBy)
import           Data.String (fromString)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as S
import           Bead.View.Content hiding (table, option)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.View.UserActions as UA (UserAction(..))

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

courseAdmin = ViewHandler courseAdminPage

data PageData = PageData {
    courses     :: [(CourseKey, Course)]
  , groups      :: [(GroupKey, String)]
  , groupAdmins :: [User]
  , assignedGroups :: [(Course,[(Group,[User])])]
  }

courseAdminPage :: GETContentHandler
courseAdminPage = do
  pageData <- userStory $ do
    theCourses <- S.administratedCourses
    courseAndGroupKeys <- forM theCourses $ \(ck,_) -> S.loadCourse ck
    theGroups <- forM courseAndGroupKeys $ \(c,gkeys) -> do
      grps <- forM gkeys S.loadGroup
      let courseWithGroups = repeat c `zip` grps
      return (gkeys `zip` (Prelude.map (uncurry fullGroupName) courseWithGroups))
    ps <- S.selectUsers group_admin
    gs <- S.groupAdministrators
    return PageData {
        courses     = theCourses
      , groups      = concat theGroups
      , groupAdmins = ps
      , assignedGroups = gs
      }
  setPageContents $ courseAdminContent pageData
  where
    group_admin = groupAdmin . u_role


-- * Create group

createGroup :: ModifyHandler
createGroup = ModifyHandler submitGroup

submitGroup :: POSTContentHandler
submitGroup = UA.CreateGroup
  <$> getParameter (jsonCourseKeyPrm (fieldName courseKeyInfo))
  <*> getGroup

-- * Assign GroupAdmin to a group

assignGroupAdmin :: ModifyHandler
assignGroupAdmin = ModifyHandler submitGroupAdmin

submitGroupAdmin :: POSTContentHandler
submitGroupAdmin = UA.CreateGroupAdmin
  <$> getParameter (jsonUsernamePrm (fieldName selectedGroupAdmin))
  <*> getParameter (jsonGroupKeyPrm (fieldName selectedGroup))

-- * View

groupAdministratorsTable :: I18N -> [(Course, [(Group, [User])])] -> H.Html
groupAdministratorsTable _ [] = return ()
groupAdministratorsTable i18n cgroups = do
  let cgroups' = sortBy (compare `on` (courseName . fst)) cgroups
  forM_ cgroups $ \(course, groups) -> when (not $ null groups) $ do
    let groups' = sortBy (compare `on` (groupName . fst)) groups
        cname   = courseName course
    Bootstrap.row $ Bootstrap.colMd12  $ Bootstrap.table $ do
      H.thead $ tr $ H.th $ fromString cname
      H.thead $ tr $ do
        H.th (fromString . i18n $ msg_CourseAdmin_GroupAdmins_Group "Group")
        H.th (fromString . i18n $ msg_CourseAdmin_GroupAdmins_Admins "Group Admins")
      H.tbody $ forM_ groups' $ \(group, admins) -> do
        H.tr $ do
          H.td (fromString $ groupName group)
          H.td (fromString . concat . intersperse ", " $ Prelude.map u_name admins)

courseAdminContent :: PageData -> IHtml
courseAdminContent info = do
  msg <- getI18N
  return $ do
    Bootstrap.row $ do
      -- New Group for the course
      Bootstrap.colMd12 $ do
        H.h3 $ (fromString $ msg $ msg_CourseAdmin_CreateGroup "New group for the course")
        nonEmpty (courses info) (H.p $ fromString $ msg $ msg_CourseAdmin_NoCourses "There are no courses.") $
          postForm (routeOf createGroup) $ do
            -- Hidden message
            H.span ! A.id (fieldName pctHelpMessage) ! A.hidden "" $
              (fromString $ msg $ msg_CourseAdmin_PctHelpMessage "Minimum of percent to achieve by students")
            Bootstrap.selection (fieldName courseKeyInfo) (const False) courses'
            Bootstrap.textInput (fieldName groupNameField) (msg $ msg_Input_Group_Name "Title") ""
            Bootstrap.textInput (fieldName groupDescField) (msg $ msg_Input_Group_Description "Description") ""
            Bootstrap.submitButton (fieldName createGroupBtn) (fromString $ msg $ msg_CourseAdmin_CreateCourse "Create group")
            hr
      -- Assign teacher to the group
      Bootstrap.colMd12 $ do
        H.h3 $ (fromString $ msg $ msg_CourseAdmin_AssignAdmin "Assign teacher to the group")
        nonEmpty (groups info) (H.p $ fromString . msg $ msg_CourseAdmin_NoGroups "There are no groups.") $
          nonEmpty (groupAdmins info) (H.p $ fromString . msg $ msg_CourseAdmin_NoGroupAdmins "There are no teachers.") $
          postForm (routeOf assignGroupAdmin) $ do
            Bootstrap.selection (fieldName selectedGroup) (const False) groups'
            Bootstrap.selection (fieldName selectedGroupAdmin) (const False) groupAdmins'
            Bootstrap.submitButton (fieldName assignGroupAdminBtn) (fromString $ msg $ msg_CourseAdmin_AssignAdmin_Button "Assign")

    Bootstrap.row $ Bootstrap.colMd12 $ hr
    Bootstrap.row $ Bootstrap.colMd12 $ p $
      fromString $ msg $ msg_CourseAdmin_GroupAdmins_Info
        "The following table(s) contain(s) the course related groups and the username of the group admins."

    -- Group Administrators table
    groupAdministratorsTable msg (assignedGroups info)

  where
    courses' :: [(CourseKey, String)]
    courses' = Prelude.map (Prelude.id *** courseName) (courses info)

    groups' :: [(GroupKey, String)]
    groups' = (groups info)

    groupAdmins' :: [(Username, String)]
    groupAdmins' = sortBy (compareHun `on` snd) $ Prelude.map (u_username &&& userLongname) (groupAdmins info)

    userLongname :: User -> String
    userLongname u = concat [ u_name u, " - ", usernameCata Prelude.id $ u_username u ]

    createGroup = Pages.createGroup ()
    assignGroupAdmin = Pages.assignGroupAdmin ()

getGroup = Group
  <$> getParameter (stringParameter (fieldName groupNameField) "Group name")
  <*> getParameter (stringParameter (fieldName groupDescField) "Group description")
