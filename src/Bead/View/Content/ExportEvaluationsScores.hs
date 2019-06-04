{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.ExportEvaluationsScores
  ( exportEvaluationsScores
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
import           Data.String.Utils (replace)
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
import           Bead.Domain.String (removeAccents)
import           Bead.View.Content
import           Bead.View.ContentHandler (Mime(MimeZip))
import qualified Bead.View.Content.ScoreInfo as ScoreI
import qualified Bead.View.Content.SubmissionState as SbmState
import           Bead.View.Content.ExportSubmissions (localTimeInSeconds)
import           Bead.View.RequestParams

exportEvaluationsScoresAllGroups :: DataHandler
exportEvaluationsScoresAllGroups = DataHandler $ do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  (course, groups) <- userStory $ do
    crs <- fst <$> S.loadCourse ck
    gks <- S.groupsOfCourse ck
    grps <- forM gks $ \gk -> do
              grp <- S.loadGroup gk
              return (gk, grp, groupName grp)
    return (crs, grps)
  exportEvaluationsScoresOfGroups course groups

exportEvaluationsScores :: DataHandler
exportEvaluationsScores = DataHandler $ do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  (course, groups) <- userStory $
    (,) <$> (fst <$> S.loadCourse ck) <*> S.administratedGroups
  exportEvaluationsScoresOfGroups course groups

exportEvaluationsScoresOfGroups :: Course -> [(GroupKey, Group, String)] -> ContentHandler File
exportEvaluationsScoresOfGroups course groups = do
  msg <- i18nE
  convertToLocalTime <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ convertToLocalTime <$> getCurrentTime
  (evaluations, scoreBoards) <- userStory $ do
    scoreBs <- mapM (S.scoreBoardOfGroup . fst3) groups
    evals <- mapM (S.groupSubmissionTable . fst3) groups
    return (evals, scoreBs)
  downloadEvaluations
    (removeAccents (courseName course) <.> "zip")
    now
    (map (first (removeAccents)) (evaluationCsvs msg groups evaluations ++ assessmentCsvs msg groups scoreBoards))

replaceSlash :: String -> String
replaceSlash = replace "/" "_"

makeFilePath :: String -> String -> FilePath
makeFilePath folder filename = replaceSlash folder </> replaceSlash filename

evaluationCsvs :: I18N -> [(GroupKey, Group, String)] -> [SubmissionTableInfo] -> [(FilePath, Text)]
evaluationCsvs msg groups submissionTables = map submissionTableToCsv (filter hasAssignment submissionTables)
  where
    hasAssignment :: SubmissionTableInfo -> Bool
    hasAssignment = submissionTableInfoCata
                       (\_ _ assignments _ _ -> not . null $ assignments)
                       (\_ _ assignments _ _ _ -> not . null $ assignments)

    submissionTableToCsv :: SubmissionTableInfo -> (FilePath, Text)
    submissionTableToCsv submissionTable = (filename, T.unlines (header : map userLine (L.sortBy (compareHun `on` (ud_fullname . fst)) (stiUserLines submissionTable))))
      where
        filename :: FilePath
        filename = concat
                     [ submissionTableInfoCata
                         (\course _ _ _ _ -> replaceSlash course)
                         (\course _ _ _ _ gk -> maybe
                                                    (replaceSlash course)
                                                    (\(_, grp, _) -> replaceSlash (E.groupName grp))
                                                    (L.find (\(gk',_,_) -> gk == gk') groups))
                         submissionTable
                     , "_"
                     , msg (msg_ExportEvaluations_Evaluations "evaluations")
                     ] <.> "csv"

        as :: [(AssignmentKey, Assignment, HasTestCase)]
        as = submissionTableInfoCata course group submissionTable
          where
            course _ _ as _ _ = as
            group _ _ as _ _ _ = map (cgInfoCata id id) as

        header :: Text
        header = T.intercalate "," ("" : "" : map (T.pack . A.name . snd3) as)

        userLine :: (UserDesc, Map AssignmentKey (SubmissionKey, SubmissionState)) -> Text
        userLine (uDesc, evaluations) = T.intercalate "," (T.pack (ud_fullname uDesc) : uid : map getEvaluation as)

          where
            uid :: Text
            uid = E.uid T.pack (ud_uid uDesc)

            getEvaluation :: (AssignmentKey, Assignment, HasTestCase) -> Text
            getEvaluation (ak, _, _) = maybe "" (\(_, st) -> SbmState.formatSubmissionState SbmState.toPlainText msg st) (M.lookup ak evaluations)

assessmentCsvs :: I18N -> [(GroupKey, Group, String)] -> [ScoreBoard] -> [(FilePath, Text)]
assessmentCsvs msg groups scoreBoards = map scoreBoardToCsv (filter (not . null . sbAssessments) scoreBoards)
  where
    scoreBoardToCsv :: ScoreBoard -> (FilePath, Text)
    scoreBoardToCsv board = (fileName, T.unlines (header : userLines board))
      where
        fileName :: FilePath
        fileName = concat
                     [ scoreBoardCata
                       (\_ _ _ courseName _ _ -> replaceSlash courseName)
                       (\_ _ _ groupName _ _ -> replaceSlash groupName)
                       board
                     , "_"
                     , msg $ msg_ExportEvaluations_Assessments "assessments"
                     ] <.> "csv"

        header :: Text
        header = T.intercalate "," ("" : "" : map (T.pack . Assess.title . snd) (sbAssessments board))

    userLines :: ScoreBoard -> [Text]
    userLines board = map userLine (L.sortBy (compareHun `on` ud_fullname) (sbUsers board))
      where
        userLine :: UserDesc -> Text
        userLine uDesc = T.intercalate "," (fullname : uid : scores)
          where
            uid :: Text
            uid = E.uid T.pack (ud_uid uDesc)

            fullname :: Text
            fullname = T.pack . ud_fullname $ uDesc

            scores :: [Text]
            scores = map (score . fst) (sbAssessments board)
              where
                score :: AssessmentKey -> Text
                score ak = T.pack $ maybe notFound (\scr -> ScoreI.scoreInfoToText notFound msg scr) $ do
                  sk <- M.lookup (ak, ud_username uDesc) (sbScores board)
                  M.lookup sk (sbScoreInfos board)

                notFound :: String
                notFound = ""

downloadEvaluations :: String -> LocalTime -> [(FilePath, Text)] -> ContentHandler File
downloadEvaluations _ _ [] = undefined
downloadEvaluations _ _ [(filename, contents)] = downloadText filename contents
downloadEvaluations filename modificationTime files = downloadLazy filename MimeZip . Zip.fromArchive $ foldr addEntry Zip.emptyArchive files

  where
    addEntry :: (FilePath, Text) -> Zip.Archive -> Zip.Archive
    addEntry (p, contents) archive = Zip.addEntryToArchive (Zip.toEntry p modTimeInSeconds (BL.fromStrict $ TE.encodeUtf8 contents)) archive

    modTimeInSeconds :: Integer
    modTimeInSeconds = localTimeInSeconds modificationTime
