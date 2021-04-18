{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Notifications.Page (
    notifications
  ) where

import           Control.Monad (forM_)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Blaze as B
import           Text.Blaze.Html5 as H hiding (link, map)
import           Text.Printf (printf)

import qualified Bead.Controller.Pages as Pages
import           Bead.View.Content
import           Bead.Domain.Entity.Notification
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.Controller.UserStories as Story (notifications)

data PageData = PageData {
      pdNotifications :: [(Notification, NotificationState, NotificationReference)]
    , pdUserTime      :: UserTimeConverter
    }

notifications :: ViewHandler
notifications = ViewHandler notificationsPage

notificationsPage :: GETContentHandler
notificationsPage = do
  pd <- PageData <$> (userStory Story.notifications) <*> userTimeZoneToLocalTimeConverter
  setPageContents $ htmlPage (msg_LinkText_Notifications "Notifications") $ notificationsContent pd

notificationsContent :: PageData -> IHtml
notificationsContent p = do
  msg <- getI18N
  return $ do
    let notifs = pdNotifications p
    if (null notifs)
      then do
         H.p $ B.toMarkup $ msg $
           msg_Notifications_NoNotifications "There are no notifications."
      else do
        Bootstrap.row $ Bootstrap.colMd12 $ do
          Bootstrap.listGroup $
            forM_ notifs $ \(notif, state, ref) ->
              (if state == Seen
                then Bootstrap.listGroupLinkItem
                else Bootstrap.listGroupAlertLinkItem Bootstrap.Info
              ) (linkFromNotif ref) . B.toMarkup $
                  T.unwords
                    [ T.concat ["[", (T.pack . showDate . pdUserTime p $ notifDate notif), "]"]
                    , translateMessage msg (translateEvent $ notifEvent notif)
                    ]

linkFromNotif :: NotificationReference -> Text
linkFromNotif = notificationReference
  (\ak sk ck  -> routeWithAnchor (Pages.submissionDetails ak sk ()) ck)
  (\ak sk _ek -> routeWithAnchor (Pages.submissionDetails ak sk ()) SubmissionDetailsEvaluationDiv)
  (\sk _ek   -> routeOf $ Pages.viewUserScore sk ())
  (\ak -> routeOf $ Pages.submission ak ())
  (\ak -> routeOf $ Pages.viewAssessment ak ())
  (routeOf $ Pages.notifications ()) -- System notifications are one liners

-- Resolve a notification event to an actual message through the I18N layer.
translateEvent :: NotificationEvent -> TransMsg
translateEvent e = case e of
  NE_CourseAdminCreated course -> TransPrmMsg
    (msg_NE_CourseAdminCreated "A course has been assigned: %s")
      course
  NE_CourseAdminAssigned course assignee -> TransPrm2Msg
    (msg_NE_CourseAdminAssigned "An administrator has been added to course \"%s\": %s")
      course (T.pack assignee)
  NE_TestScriptCreated creator course -> TransPrm2Msg
    (msg_NE_TestScriptCreated "%s created a new test script for course \"%s\"")
      (T.pack creator) course
  NE_TestScriptUpdated editor script course -> TransPrm3Msg
    (msg_NE_TestScriptUpdated "%s modified test script \"%s\" for course \"%s\"")
      (T.pack editor) script course
  NE_RemovedFromGroup group deletor -> TransPrm2Msg
    (msg_NE_RemovedFromGroup "Removed from group \"%s\" by %s")
      group (T.pack deletor)
  NE_GroupAdminCreated course creator group -> TransPrm3Msg
    (msg_NE_GroupAdminCreated "A group of course \"%s\" has been assigned by %s: %s")
      course (T.pack creator) group
  NE_GroupAssigned group course assignor assignee -> TransPrm4Msg
    (msg_NE_GroupAssigned "Group \"%s\" of course \"%s\" has been assigned to %s by %s")
      group course (T.pack assignor) (T.pack assignee)
  NE_GroupCreated course creator group -> TransPrm3Msg
    (msg_NE_GroupCreated "A group has been created for course \"%s\" by %s: %s")
      course (T.pack creator) group
  NE_GroupAssignmentCreated creator group course assignment -> TransPrm4Msg
    (msg_NE_GroupAssignmentCreated "%s created a new assignment for group \"%s\" (\"%s\"): %s")
      (T.pack creator) group course assignment
  NE_CourseAssignmentCreated creator course assignment -> TransPrm3Msg
    (msg_NE_CourseAssignmentCreated "%s created a new assignment for course \"%s\": %s")
      (T.pack creator) course assignment
  NE_GroupAssessmentCreated creator group course assessment -> TransPrm4Msg
    (msg_NE_GroupAssessmentCreated "%s created a new assessment for group \"%s\" (\"%s\"): %s")
      (T.pack creator) group course assessment
  NE_CourseAssessmentCreated creator course assessment -> TransPrm3Msg
    (msg_NE_CourseAssessmentCreated "%s created a new assessment for course \"%s\": %s")
      (T.pack creator) course assessment
  NE_AssessmentUpdated editor assessment -> TransPrm2Msg
    (msg_NE_AssessmentUpdated "%s modified assessment: %s")
      (T.pack editor) assessment
  NE_AssignmentUpdated editor assignment -> TransPrm2Msg
    (msg_NE_AssignmentUpdated "%s modified assignment: %s")
      (T.pack editor) assignment
  NE_EvaluationCreated evaluator submission -> TransPrm2Msg
    (msg_NE_EvaluationCreated "%s evaluated submission: %s")
      (T.pack evaluator) (T.pack submission)
  NE_AssessmentEvaluationUpdated editor assessment -> TransPrm2Msg
    (msg_NE_AssessmentEvaluationUpdated "%s modified evaluation of score: %s")
      (T.pack editor) (T.pack assessment)
  NE_AssignmentEvaluationUpdated editor submission -> TransPrm2Msg
    (msg_NE_AssignmentEvaluationUpdated "%s modified evaluation of submission: %s")
      (T.pack editor) (T.pack submission)
  NE_CommentCreated commenter submission body -> TransPrm3Msg
    (msg_NE_CommentCreated "%s commented on submission %s: \"%s\"")
      commenter (T.pack submission) body
