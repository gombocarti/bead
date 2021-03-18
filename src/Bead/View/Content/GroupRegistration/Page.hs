{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.GroupRegistration.Page (
    groupRegistration
  , unsubscribeFromCourse
  ) where

import           Control.Monad
import           Data.Function (on)
import qualified Data.HashSet as HashSet
import           Data.List (intercalate, sortBy)
import           Data.String (fromString)

import           Text.Blaze.Html5 as H hiding (map)

import           Bead.Controller.UserStories (availableGroups, attendedGroups)
import qualified Bead.Controller.UserStories as Story
import qualified Bead.Controller.Pages as Pages
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap

groupRegistration = ViewModifyHandler groupRegistrationPage postGroupReg

unsubscribeFromCourse = ModifyHandler $ do
  gk <- getParameter unsubscribeUserGroupKeyPrm
  return $ Action $ do
    Story.unsubscribeFromCourse gk
    return $ redirection $ Pages.welcome ()

data GroupRegData = GroupRegData {
    groups :: [(Course, GroupKey, Group, [User])]
  , groupsRegistered :: [(Course, GroupKey, Group, [User], Bool)]
  }

postGroupReg :: POSTContentHandler
postGroupReg = do
  gk <- getParameter (jsonGroupKeyPrm (fieldName groupRegistrationField))
  return $ Action $ do
    success <- Story.subscribeToGroup gk
    return $ redirection $
      if success
      then Pages.studentView gk ()
      else Pages.groupRegistration ()

groupRegistrationPage :: GETContentHandler
groupRegistrationPage = do
  desc <- userStory $ do
    as <- attendedGroups
    let attendedGroupKeys = HashSet.fromList $ map snd5 as
        newGroupForUser (_, gk, _, _) = not (HashSet.member gk attendedGroupKeys)
    gs <- (filter newGroupForUser) <$> availableGroups
    return GroupRegData {
        groups = gs
      , groupsRegistered = as
      }
  setPageContents $ htmlPage (msg_LinkText_GroupRegistration "Group Registration") $ groupRegistrationContent desc
  where
    snd5 (_,b,_,_,_) = b

groupRegistrationContent :: GroupRegData -> IHtml
groupRegistrationContent desc = do
  msg <- getI18N
  return $ do
    let registeredGroups = groupsRegistered desc
    Bootstrap.rowColMd12 $ do
      H.h3 $ fromString $ msg $ msg_GroupRegistration_RegisteredCourses "Registered courses"
      i18n msg $ groupsAlreadyRegistered registeredGroups
    when (not . null $ registeredGroups) $ Bootstrap.rowColMd12 $ do
      H.p $ (fromString . msg $ msg_GroupRegistration_Warning $ concat
        [ "It is possible to quit from a group or move between groups until a submission is "
        , "submitted.  Otherwise, the teacher of the given group should be asked to undo the "
        , "group registration."
        ])
    Bootstrap.rowColMd12 $ do
      H.h3 $ (fromString . msg $ msg_GroupRegistration_NewGroup "New group")
    i18n msg $ groupsForTheUser (groups desc)

groupsAlreadyRegistered :: [(Course, GroupKey, Group, [User], Bool)] -> IHtml
groupsAlreadyRegistered ds = do
  msg <- getI18N
  return $ nonEmpty ds
    (fromString . msg $ msg_GroupRegistration_NoRegisteredCourses
      "No registered courses.  Choose a group.")
    (Bootstrap.table $ do
      thead $ H.tr $ do
        H.th . fromString . msg $ msg_GroupRegistration_Group "Group"
        H.th . fromString . msg $ msg_GroupRegistration_Admin "Teacher"
        H.th . fromString . msg $ msg_GroupRegistration_Unsubscribe "Unregister"
      tbody $ mapM_ (groupLine msg) ds)
  where
    unsubscribeFromCourse k = Pages.unsubscribeFromCourse k ()

    groupLine msg (course, key, grp, admins, hasSubmission) = do
      H.tr $ do
        H.td $ fromString (fullGroupName course grp)
        H.td $ fromString $ intercalate ", " (map u_name . sortUsersByName $ admins)
        H.td $
          if hasSubmission
            then (fromString . msg $ msg_GroupRegistration_NoUnsubscriptionAvailable
              "Unregistration is not allowed.")
            else postForm (routeOf $ unsubscribeFromCourse key) $
                   Bootstrap.smallSubmitButton
                     (fieldName unsubscribeFromCourseSubmitBtn)
                     (msg $ msg_GroupRegistration_Unsubscribe "Unregister")

groupsForTheUser :: [(Course, GroupKey, Group, [User])] -> IHtml
groupsForTheUser gs = do
  msg <- getI18N
  return $
    nonEmpty gs
      (Bootstrap.rowColMd12 $ p $ fromString . msg $ msg_GroupRegistration_NoAvailableCourses "There are no available groups yet.") $
      postForm (routeOf groupRegistration) $ do
        Bootstrap.selectionWithPlaceholder
          (fieldName groupRegistrationField)
          (msg $ msg_GroupRegistration_SelectGroup "Select course and group")
          (map (\(c, gk, g, admins) -> (gk, descriptive c g admins)) . sortCoursesAndAdmins $ gs)
        Bootstrap.submitButton (fieldName regGroupSubmitBtn) (msg $ msg_GroupRegistration_Register "Register")
  where
    groupRegistration = Pages.groupRegistration ()

    descriptive :: Course -> Group -> [User] -> String
    descriptive c g admins = unwords [fullGroupName c g, "/", intercalate ", " (map u_name admins)]

    sortCoursesAndAdmins :: [(Course, GroupKey, Group, [User])] -> [(Course, GroupKey, Group, [User])]
    sortCoursesAndAdmins = sortBy (compareHun `on` (courseName . fst4)) . map (\(c, gk, g, admins) -> (c, gk, g, sortUsersByName admins))
      where
        fst4 :: (a, b, c, d) -> a
        fst4 (a, _, _, _) = a

sortUsersByName :: [User] -> [User]
sortUsersByName = sortBy (compareHun `on` u_name)
