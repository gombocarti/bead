{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.EvaluationTable.Page (
    evaluationTable
  ) where

import           Control.Monad
import           Data.Function (on)
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Tuple.Utils (snd3, thd3)

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (openSubmissions)
import           Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Pagelets
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.View.Content.StateVisualization as SV

import qualified Text.Blaze as B
import           Text.Blaze.Html5 as H hiding (link, map)
import qualified Text.Blaze.Html5.Attributes as A

evaluationTable :: ViewHandler
evaluationTable = ViewHandler evaluationTablePage

evaluationTablePage :: GETContentHandler
evaluationTablePage = do
  evalTable <- evaluationTableContent
               <$> userTimeZoneToLocalTimeConverter
               <*> userStory openSubmissions
  setPageContents $ htmlPage (msg_LinkText_EvaluationTable "Evaluations") evalTable

evaluationTableContent :: UserTimeConverter -> OpenedSubmissions -> IHtml
evaluationTableContent tc = openedSubmissionsCata $ \admincourse admingroup related -> do
  msg <- getI18N
  return $ do
    noUnevaluatedSubmission msg admincourse admingroup related
    when (not $ null admingroup) $ Bootstrap.rowColMd12 $ do
      H.h4 $ B.toMarkup . msg $ msg_EvaluationTable_GroupAssignment "Group assignments"
      B.toMarkup . msg $ msg_EvaluationTable_GroupAssignmentInfo $ T.concat
        [ "Submissions for group assignments sent by the users who correspond "
        , "the groups administrated by you."
        ]
      evaluationTable msg (sortSubmissions admingroup) isGroup
    when (not $ null admincourse) $ Bootstrap.rowColMd12 $ do
      H.h4 $ B.toMarkup . msg $ msg_EvaluationTable_CourseAssignment "Course assignments"
      B.toMarkup . msg $ msg_EvaluationTable_CourseAssignmentInfo $ T.concat
        [ "Submissions for course assignments sent by the users who correspond "
        , "the courses of groups administrated by you."
        ]
      evaluationTable msg (sortSubmissions admincourse) isCourse
    when (not $ null related) $ Bootstrap.rowColMd12 $ do
      H.h4 $ B.toMarkup . msg $ msg_EvaluationTable_MiscCourseAssignment "Miscellaneous Course assignments"
      B.toMarkup . msg $ msg_EvaluationTable_MiscCourseAssignmentInfo $ T.concat
        [ "Submissions for course assignments sent by the users who correspond "
        , "the courses of groups administrated by you, but not your students."
        ]
      evaluationTable msg (sortSubmissions related) isCourse
  where
    isGroup  = True
    isCourse = False

    noUnevaluatedSubmission :: I18N -> [SMVal] -> [SMVal] -> [SMVal] -> H.Html
    noUnevaluatedSubmission msg ac ag rl = Bootstrap.rowColMd12 $ if (and [null ac, null ag, null rl])
      then (H.p $ B.toMarkup $ msg $ msg_EvaluationTable_EmptyUnevaluatedSolutions "There are no unevaluated submissions.")
      else
        H.p $ B.toMarkup . msg $ msg_EvaluationTable_Info $ T.concat
          [ "Only the last unevaluated submission is shown per student. The "
          , "other submissions may be accessed through the submission table "
          , "on the home page."
          ]

    evaluationTable :: I18N -> [SMVal] -> Bool -> H.Html
    evaluationTable msg ks isGroup =
      when (not $ null ks) $ Bootstrap.rowColMd12 $ do
        Bootstrap.table $ do
          thead $ H.tr $ do
            H.th (B.toMarkup . msg $ msg_EvaluationTable_Link "Link")
            H.th (B.toMarkup . msg $ msg_EvaluationTable_Assignment "Assignment")
            H.th (B.toMarkup . msg $ msg_EvaluationTable_Username "Username")
            H.th (B.toMarkup . msg $ msg_EvaluationTable_Student "Student")
            H.th (B.toMarkup . msg $ msg_EvaluationTable_Course "Course")
            when isGroup $ H.th (B.toMarkup . msg $ msg_EvaluationTable_Group "Group")
            H.th (B.toMarkup . msg $ msg_EvaluationTable_DateOfSubmission "Date")
            H.th (B.toMarkup. msg $ msg_EvaluationTable_SubmissionInfo "State")
          tbody $ forM_ ks (submissionInfo tc msg isGroup)

submissionInfo tc msg isGroup (key, desc) = H.tr $ do
  H.td $ Bootstrap.link (routeOf (evaluation key)) (msg $ msg_EvaluationTable_Solution "Submission")
  H.td . B.toMarkup . Assignment.name . eAssignment $ desc
  uid (H.td . B.toMarkup) . u_uid $ eStudent desc
  H.td . B.toMarkup . u_name . eStudent $ desc
  H.td . B.toMarkup . shortCourseName . eCourse $ desc
  when isGroup $ H.td . B.toMarkup . maybe "" shortGroupName . eGroup $ desc
  H.td . B.toMarkup . showDate . tc $ submissionPostTime desc
  H.td . SV.formatSubmissionState SV.toMediumIcon msg . snd3 . eSubmissionInfo $ desc
  where
    evaluation k = Pages.evaluation k ()

-- * Sorting submissions

-- Create an ordered submission list based on the course name, group name and assignment time ordering.
-- The submission list elements are ordered by the submission time of the solution.

-- |The key for the ordering consists of a course name, a group name (possibly empty), and
-- the time of the assignment
type SMKey = (Text, Maybe Text, UTCTime)

type SMVal = (SubmissionKey, SubmissionDesc)

-- SubmissionMap an ordered map which hold several
type SMap = Map SMKey [SMVal]

descToKey :: SMVal -> SMKey
descToKey (_k,d) = (courseName $ eCourse d, shortGroupName <$> eGroup d, eAssignmentDate d)

insertSMap :: SMVal -> SMap -> SMap
insertSMap v m =
  let key = descToKey v
  in Map.insertWith (++) key [v] m

sortSubmissions :: [SMVal] -> [SMVal]
sortSubmissions [] = []
sortSubmissions [s] = [s]
sortSubmissions sm = concatMap (sortOn (submissionPostTime . snd)) . Map.elems . foldr insertSMap Map.empty $ sm

submissionPostTime :: SubmissionDesc -> UTCTime
submissionPostTime = thd3 . eSubmissionInfo
