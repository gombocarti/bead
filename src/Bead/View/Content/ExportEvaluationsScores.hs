{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.ExportEvaluationsScores
  ( exportEvaluationsScoresAdminedGroups
  , exportEvaluationsScoresAllGroups
  ) where

import qualified Codec.Archive.Zip as Zip
import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (getCurrentTime, LocalTime)
import           Data.Tuple.Utils (fst3, snd3, thd3)
import           System.FilePath ((<.>), (</>))

import qualified Bead.Controller.UserStories as S
import qualified Bead.Domain.Entity.Assessment as Assess
import qualified Bead.Domain.Entity.Assignment as A
import qualified Bead.Domain.Entities as E
import           Bead.Domain.String (removeAccents, replaceSlash)
import           Bead.View.Content
import           Bead.View.ContentHandler (Mime(MimeZip))
import qualified Bead.View.Content.StateVisualization as SV
import           Bead.View.Content.ExportSubmissions (localTimeInSeconds)
import           Bead.View.RequestParams

exportEvaluationsScoresAllGroups :: DataHandler
exportEvaluationsScoresAllGroups = DataHandler $ do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  gks <- userStory $ S.groupsOfCourse ck
  exportEvaluationsScoresOfGroups ck gks

exportEvaluationsScoresAdminedGroups :: DataHandler
exportEvaluationsScoresAdminedGroups = DataHandler $ do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  gks <- userStory $ S.administratedGroupsOfCourse ck
  exportEvaluationsScoresOfGroups ck gks

exportEvaluationsScoresOfGroups :: CourseKey -> [GroupKey] -> ContentHandler File
exportEvaluationsScoresOfGroups ck gks = do
  (course, groups) <- userStory $ do
    crs <- fst <$> S.loadCourse ck
    grps <- forM gks $ \gk -> do
      grp <- S.loadGroup gk
      evaluations <- S.groupSubmissionTable gk
      scores <- S.scoreBoardOfGroup gk
      return (gk, grp, evaluations, scores)
    return (crs, grps)
  msg <- i18nE
  convertToLocalTime <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ convertToLocalTime <$> getCurrentTime
  downloadEvaluations
    (courseName course <> ".zip")
    now
    (map (first removeAccents) (evaluationsScoresToCsvs msg course groups))

quote :: Text -> Text
quote t = T.cons '"' (T.snoc t '"')

escapeQuotes :: Text -> Text
escapeQuotes = T.replace "\"" "\"\""

makeFilePath :: String -> String -> FilePath
makeFilePath folder filename = replaceSlash folder </> replaceSlash filename

evaluationsScoresToCsvs :: I18N -> Course -> [(GroupKey, Group, SubmissionTableInfo, ScoreBoard)] -> [(FilePath, Text)]
evaluationsScoresToCsvs msg course groups = foldr addCsvs [] groups
  where
    addCsvs :: (GroupKey, Group, SubmissionTableInfo, ScoreBoard) -> [(FilePath, Text)] -> [(FilePath, Text)]
    addCsvs (gk, g, subm, scores) csvs =
      let csvs2 = if hasAssignment subm
                  then submissionTableToCsv msg course g subm : csvs
                  else csvs
      in if hasAssessment scores
         then scoreBoardToCsv msg course g scores : csvs2
         else csvs2

    hasAssignment :: SubmissionTableInfo -> Bool
    hasAssignment = submissionTableInfoCata
                       (\_ assignments _ _ _ -> not . null $ assignments)
                       (\_ assignments _ _ _ -> not . null $ assignments)

    hasAssessment :: ScoreBoard -> Bool
    hasAssessment = not . null . sbAssessments

submissionTableToCsv :: I18N -> Course -> Group -> SubmissionTableInfo -> (FilePath, Text)
submissionTableToCsv msg course group_ submissionTable = (filename, T.unlines (header : map userLine (L.sortBy (compareHun `on` (ud_fullname . fst)) (stiUserLines submissionTable))))
  where
    filename :: FilePath
    filename = concat
                 [ submissionTableInfoCata
                   (\_ _ _ _ _ -> replaceSlash (T.unpack $ E.courseName course))
                   (\_ _ _ _ _ -> (replaceSlash (T.unpack $ E.fullGroupName course group_)))
                   submissionTable
                 , "_"
                 , T.unpack $ msg (msg_ExportEvaluations_Evaluations "evaluations")
                 ] <.> "csv"

    as :: [(AssignmentKey, Assignment, HasTestCase)]
    as = submissionTableInfoCata course group submissionTable
      where
        course _ as _ _ _ = as
        group _ as _ _ _ = map (cgInfoCata id id) as

    header :: Text
    header = T.intercalate "," ("" : "" : map (quote . escapeQuotes . A.name . snd3) as)

    userLine :: (UserDesc, [Maybe (SubmissionKey, SubmissionState)]) -> Text
    userLine (uDesc, submissions) = T.intercalate "," (T.pack (ud_fullname uDesc) : uid : map formatSubmission submissions)

      where
        uid :: Text
        uid = E.uid T.pack (ud_uid uDesc)

        formatSubmission :: Maybe (SubmissionKey, SubmissionState) -> Text
        formatSubmission = maybe "" (\(_, st) -> SV.formatSubmissionState SV.toPlainText msg st)

scoreBoardToCsv :: I18N -> Course -> Group -> ScoreBoard -> (FilePath, Text)
scoreBoardToCsv msg course grp board = (fileName, T.unlines (header : userLines board))
  where
    fileName :: FilePath
    fileName = concat
                 [ replaceSlash (T.unpack $ E.fullGroupName course grp)
                 , "_"
                 , T.unpack $ msg $ msg_ExportEvaluations_Assessments "assessments"
                 ] <.> "csv"

    header :: Text
    header = T.intercalate "," ("" : "" : map (quote . escapeQuotes . Assess.title . snd) (sbAssessments board))

    userLines :: ScoreBoard -> [Text]
    userLines board = map userLine (L.sortBy (compareHun `on` (ud_fullname . fst)) (sbUserLines board))
      where
        userLine :: (UserDesc, [Maybe ScoreInfo]) -> Text
        userLine (uDesc, scoreInfos) = T.intercalate "," (fullname : uid : scores)
          where
            uid :: Text
            uid = E.uid T.pack (ud_uid uDesc)

            fullname :: Text
            fullname = T.pack . ud_fullname $ uDesc

            scores :: [Text]
            scores = map (maybe "" (\info -> SV.formatEvResult SV.toPlainText msg (evaluationOfInfo info))) scoreInfos

downloadEvaluations :: Text -> LocalTime -> [(FilePath, Text)] -> ContentHandler File
downloadEvaluations _ _ [] = undefined
downloadEvaluations _ _ [(filename, contents)] = downloadText (T.pack filename) contents
downloadEvaluations filename modificationTime files = downloadLazy filename MimeZip . Zip.fromArchive $ foldr addEntry Zip.emptyArchive files

  where
    addEntry :: (FilePath, Text) -> Zip.Archive -> Zip.Archive
    addEntry (p, contents) archive = Zip.addEntryToArchive (Zip.toEntry p modTimeInSeconds (BL.fromStrict $ TE.encodeUtf8 contents)) archive

    modTimeInSeconds :: Integer
    modTimeInSeconds = localTimeInSeconds modificationTime
