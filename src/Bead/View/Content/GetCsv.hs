{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.GetCsv (
    getGroupCsv 
  , getCourseCsv
  ) where

import           Bead.View.RequestParams (groupKeyParamName,courseKeyParamName)
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content
import           Bead.View.Content.StateVisualization (formatEvResult, toPlainText)
import qualified Bead.View.ContentHandler as CH

import           Control.Monad (forM)
import           Control.Monad.Trans (lift)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Function (on)
import           Data.String (fromString)
import           Data.List (sortBy,intercalate)
import           System.FilePath (FilePath, (<.>))

getGroupCsv :: DataHandler
getGroupCsv = DataHandler $ do
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  group <- userStory $ do
    Story.isAdminOfGroupOrCourse gk
    Story.loadGroup gk
  maybeAk  <- getOptionalParameter assessmentKeyPrm
  msg <- i18nE
  let filename = groupName group <.> "csv"
  case maybeAk of
    Just ak -> do
      groupScores <- userStory (Story.scoresOfGroup gk ak)
      downloadText filename (csvFilled msg groupScores)
    Nothing -> do 
      users <- userStory $ do
        usernames <- Story.subscribedToGroup gk
        mapM Story.loadUserDesc usernames
      downloadText filename (csvEmpty msg users)

getCourseCsv :: DataHandler
getCourseCsv = DataHandler $ do
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  course <- userStory $ do
    Story.isAdministratedCourse ck
    fst <$> Story.loadCourse ck
  maybeAk  <- getOptionalParameter assessmentKeyPrm
  msg <- i18nE
  let filename = courseName course <.> "csv"
  case maybeAk of
    Just ak -> do
      courseScores <- userStory (Story.scoresOfCourse ck ak)      
      downloadText filename (csvFilled msg courseScores)
    Nothing -> do
      users <- userStory $ do
        usernames <- Story.subscribedToCourse ck
        mapM Story.loadUserDesc usernames
      downloadText filename (csvEmpty msg users)

csvHeader :: I18N -> Text
csvHeader msg = information msg `T.append` header `T.append` "\n"
  where
    header = T.intercalate "," [name,username,score]
      where
        name = T.pack . msg . msg_GetCsv_StudentName $ "Name"
        username = T.pack . msg . msg_GetCsv_Username $ "Username"
        score = T.pack . msg . msg_GetCsv_Score $ "Score"

csvEmpty :: I18N -> [UserDesc] -> Text
csvEmpty msg users = csvHeader msg `T.append` body
    where
      body = T.unlines $ map line (sortBy (compareHun `on` ud_fullname) users)

      line user = userLine user ""

csvFilled :: I18N -> [(UserDesc, Maybe ScoreInfo)] -> Text
csvFilled msg users = csvHeader msg `T.append` body
    where
      body = T.unlines $ map line (sortBy (compareHun `on` (ud_fullname . fst)) users)

      line (user, Just scoreInfo) = userLine user (formatEvResult toPlainText msg (evaluationOfInfo scoreInfo))
      line (user, Nothing) = userLine user ""

userLine :: UserDesc -> Text -> Text
userLine u t = T.intercalate "," [fullName u, userid u, t]
  where
    userid = T.pack . uid id . ud_uid
    fullName = T.pack . ud_fullname

information :: I18N -> Text
information msg = T.pack . msg . msg_GetCsv_Information $ unlines
              [ "# Lines starting with '#' will be ignored."
              , "# The following scores are valid:"
              , "#  - In case of binary evaluation:"
              , "#      Accepted may be written as '+', '1' or 'Accepted'."
              , "#      Rejected may be written as '-', '0' or 'Rejected'."
              , "#      Interpretation is case in-sensitive."
              , "#  - In case of percentage evaluation: an integer from 0 to 100, inclusive."
              , "#  - In case of free form evaluation: text ending with newline character."
              ]
