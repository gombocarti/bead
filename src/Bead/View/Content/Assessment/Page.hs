{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Assessment.Page (
    newGroupAssessment
  , newCourseAssessment
  , fillNewGroupAssessmentPreview
  , fillNewCourseAssessmentPreview
  , modifyAssessment
  , modifyAssessmentPreview
  , viewAssessment
  ) where

import           Bead.View.Content
import           Bead.View.Content.Bootstrap ((.|.))
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.RequestParams
import           Bead.View.Content.StateVisualization (formatEvResultMaybe, toLargeIcon)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.Domain.Shared.Evaluation
import           Bead.Config (maxUploadSizeInKb)
import           Bead.Domain.Types (readMaybe)

import           Snap.Util.FileUploads
import           System.Directory (doesFileExist)

import           Data.Char (toUpper,isSpace)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BsUTF8 (toString)
import           Data.Function (on)
import qualified Data.Map as M
import           Data.List (sortBy,intercalate)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!))
import           Control.Monad (join,when,void)
import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class (liftIO)

-- * Content Handlers

newGroupAssessment = ViewModifyHandler newGroupAssessmentPage postNewGroupAssessment
newCourseAssessment = ViewModifyHandler newCourseAssessmentPage postNewCourseAssessment
fillNewGroupAssessmentPreview = UserViewHandler fillNewGroupAssessmentPreviewPage
fillNewCourseAssessmentPreview = UserViewHandler fillNewCourseAssessmentPreviewPage
modifyAssessment = ViewModifyHandler modifyAssessmentPage postModifyAssessment
modifyAssessmentPreview = UserViewHandler modifyAssessmentPreviewPage
viewAssessment = ViewHandler viewAssessmentPage

type Title = Text
type Description = Text

data PageData = PD_NewCourseAssessment CourseKey
              | PD_NewGroupAssessment GroupKey
              | PD_PreviewCourseAssessment CourseKey Title Description EvConfig [UserDesc] (M.Map UserDesc Evaluation)
              | PD_PreviewGroupAssessment GroupKey Title Description EvConfig [UserDesc] (M.Map UserDesc Evaluation)
              | PD_ModifyAssessment {
                  pdAKey             :: AssessmentKey
                , pdAs               :: Assessment
                , courseOrGroupKey   :: Either CourseKey GroupKey
                , pdIsScoreSubmitted :: Bool
                }
              | PD_ModifyAssessmentPreview {
                  pdAKey             :: AssessmentKey
                , pdAs               :: Assessment
                , courseOrGroupKey   :: Either CourseKey GroupKey
                , pdIsScoreSubmitted :: Bool
                , users              :: [UserDesc]
                , pdEvaluations      :: M.Map UserDesc Evaluation
                } 

pageDataAlgebra
  newCourseAssessment
  newGroupAssessment
  previewCourseAssessment
  previewGroupAssessment
  modifyAssessment
  modifyAssessmentPreview
  pdata =
      case pdata of
        PD_NewCourseAssessment ck -> newCourseAssessment ck
        PD_NewGroupAssessment gk -> newGroupAssessment gk
        PD_PreviewCourseAssessment ck title description evConfig users evaluations -> previewCourseAssessment ck title description evConfig users evaluations
        PD_PreviewGroupAssessment gk title description evConfig users evaluations -> previewGroupAssessment gk title description evConfig users evaluations
        PD_ModifyAssessment ak as cGKey isScoreSubmitted -> modifyAssessment ak as cGKey isScoreSubmitted
        PD_ModifyAssessmentPreview ak as cGKey isScoreSubmitted users evaluations -> modifyAssessmentPreview ak as cGKey isScoreSubmitted users evaluations

data UploadResult
  = PolicyFailure
  | File FilePath !ByteString
  | InvalidFile
  | UnnamedFile
  deriving (Eq,Show)

newGroupAssessmentPage :: GETContentHandler
newGroupAssessmentPage = do
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  setPageContents $ htmlPage (msg_LinkText_NewGroupAssessment "New Group Assessment") $
    fillAssessmentTemplate $ PD_NewGroupAssessment gk

postNewGroupAssessment :: POSTContentHandler
postNewGroupAssessment = do 
  msg <- i18nE
  uploadResult <- uploadFile
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  evalConfig <- getParameter evConfigParam
  now <- liftIO getCurrentTime
  let visible = True
      a = Assessment title description now evalConfig visible
      redirectionTarget = Pages.groupOverview gk ()
  case uploadResult of
    [File _name contents] -> do
      users <- userStory $ do
        usernames <- Story.subscribedToGroup gk
        mapM Story.loadUserDesc usernames
      let evaluations = fromUserDescKey (toUserDescKey ud_uid users (parseEvaluations msg evalConfig (readCsv contents)))
      return $ Action $ do
        Story.saveScoresOfGroupAssessment gk a evaluations
        return $ redirection redirectionTarget
    _ -> do
      evaluations <- read <$> getParameter evaluationsParam
      return $ Action $ do
        if M.null evaluations
          then void $ Story.createGroupAssessment gk a
          else Story.saveScoresOfGroupAssessment gk a evaluations
        return $ redirection redirectionTarget

newCourseAssessmentPage :: GETContentHandler
newCourseAssessmentPage = do
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  setPageContents $ htmlPage (msg_LinkText_NewCourseAssessment "New Course Assessment") $
    fillAssessmentTemplate $ PD_NewCourseAssessment ck

postNewCourseAssessment :: POSTContentHandler
postNewCourseAssessment = do 
  msg <- i18nE
  uploadResult <- uploadFile
  title <- getParameter titleParam
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  description <- getParameter descriptionParam
  evalConfig <- getParameter evConfigParam
  now <- liftIO getCurrentTime
  let visible = True
      a = Assessment title description now evalConfig visible
      redirectionTarget = Pages.courseManagement ck Pages.AssignmentsContents ()
  case uploadResult of
    [File _name contents] -> do
      users <- userStory $ do
        usernames <- Story.subscribedToCourse ck
        mapM Story.loadUserDesc usernames
      let evaluations = fromUserDescKey (toUserDescKey ud_uid users (parseEvaluations msg evalConfig (readCsv contents)))
      return $ Action $ do
        Story.saveScoresOfCourseAssessment ck a evaluations
        return $ redirection redirectionTarget
    _ -> do
      evaluations <- read <$> getParameter evaluationsParam
      return $ Action $ do
        if M.null evaluations
          then void $ Story.createCourseAssessment ck a
          else Story.saveScoresOfCourseAssessment ck a evaluations
        return $ redirection redirectionTarget

toUserDescKey :: Ord k => (UserDesc -> k) -> [UserDesc] -> M.Map k a -> M.Map UserDesc a
toUserDescKey select users m = foldr f M.empty users
    where 
      f user acc = maybe acc (\a -> M.insert user a acc) (M.lookup (select user) m)

fromUserDescKey :: M.Map UserDesc a -> M.Map Username a
fromUserDescKey = M.mapKeys ud_username

parseEvaluations :: I18N -> EvConfig -> M.Map Uid String -> M.Map Uid Evaluation
parseEvaluations msg evalConfig = M.mapMaybe (parseEvaluation msg evalConfig)

parseEvaluation :: I18N -> EvConfig -> String -> Maybe Evaluation
parseEvaluation _msg _evalConfig "" = Nothing
parseEvaluation msg evalConfig s = evConfigCata 
                                 (mkEval <$> readBinary)
                                 (\_ -> mkEval <$> readPercentage)
                                 (Just . mkEval . freeFormResult . T.pack $ s)
                                 evalConfig
    where mkEval :: EvResult -> Evaluation
          mkEval result = Evaluation result ""

          readBinary :: Maybe EvResult
          readBinary | isAccepted = Just . binaryResult $ Passed
                     | isRejected = Just . binaryResult $ Failed
                     | otherwise  = Nothing
              where
                isAccepted = normalized `elem` [accepted,"+","1"]
                isRejected = normalized `elem` [rejected,"-","0"]
                normalized = T.map toUpper (T.strip . T.pack $ s)
                accepted   = T.map toUpper (msg . msg_NewAssessment_Accepted $ "Accepted")
                rejected   = T.map toUpper (msg . msg_NewAssessment_Rejected $ "Rejected")

          readPercentage :: Maybe EvResult
          readPercentage = case readMaybe s :: Maybe Int of
                             Just p -> if p >= 0 && p <= 100
                                       then Just . percentageResult . (/100) . fromIntegral $ p
                                       else Nothing
                             Nothing -> Nothing

titleParam = textParameter "n1" "Title"
descriptionParam = textParameter "n2" "Description"
evaluationsParam = stringParameter "evaluations" "Evaluations"
evConfigParam = evalConfigParameter "evConfig"

fillNewGroupAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewGroupAssessmentPreviewPage = do
  msg <- i18nE
  uploadResult <- uploadFile
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  evConfig <- getParameter evConfigParam
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  let [File _name contents] = uploadResult
  users <- userStory $ do
    usernames <- Story.subscribedToGroup gk
    mapM Story.loadUserDesc usernames
  let evaluations = toUserDescKey ud_uid users (parseEvaluations msg evConfig (readCsv contents))
  setPageContents $ htmlPage (msg_LinkText_GroupAssessmentPreview "New Group Assessment") $
    fillAssessmentTemplate $ PD_PreviewGroupAssessment gk title description evConfig users evaluations

uploadFile :: ContentHandler [UploadResult]
uploadFile = do
  results <- beadHandler $ do
    tmpDir <- getTempDirectory
    size <- maxUploadSizeInKb <$> getConfiguration
    let maxSize = fromIntegral (size * 1024)
    let uploadPolicy = setMaximumFormInputSize maxSize defaultUploadPolicy
    let perPartUploadPolicy = const $ allowWithMaximumSize maxSize
    handleFileUploads tmpDir uploadPolicy perPartUploadPolicy handlePart
  return $ filter isFile results
        where
          handlePart _partInfo (Left _exception) = return PolicyFailure
          handlePart partInfo (Right filePath) =
            case (partFileName partInfo) of
              Just fp | not (B.null fp) -> do
                contents <- liftIO $ do
                  exists <- doesFileExist filePath
                  if exists
                    then do
                      body <- B.readFile filePath
                      return $ Just body
                    else return $ Nothing
                return $ case contents of
                  Just body -> File (B.unpack fp) body
                  _         -> InvalidFile
              _         -> return UnnamedFile

          isFile :: UploadResult -> Bool
          isFile (File _ _) = True
          isFile _          = False
                         
fillNewCourseAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewCourseAssessmentPreviewPage = error "fillNewCourseAssessmentPreviewPage is undefined"

fillAssessmentTemplate :: PageData -> IHtml
fillAssessmentTemplate pdata = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do
      H.form ! A.method "post" $ do
        Bootstrap.textInputWithDefault "n1" (titleLabel msg) title
        Bootstrap.optionalTextInputWithDefault "n2" (descriptionLabel msg) description
        if readOnlyEvType
          then showEvaluationType msg selectedEvType
          else evTypeSelection msg selectedEvType
        Bootstrap.formGroup $ optionalFileInput "csv"
        Bootstrap.row $ do
          Bootstrap.colMd4 (previewButton msg ! A.disabled "")
          Bootstrap.colMd4 (downloadCsvButton msg)
          Bootstrap.colMd4 (commitButton msg)
        let evaluationsName = "evaluations" :: Text
            csvTable users evaluations = do
              previewTable msg users evaluations
              hiddenInput evaluationsName (show (fromUserDescKey evaluations))
            noPreview = hiddenInput evaluationsName (show (M.empty :: M.Map Username Evaluation))
            
        pageDataAlgebra
          (\_ -> noPreview)
          (\_ -> noPreview)
          (\_ _ _ _ users evaluations -> csvTable users evaluations)
          (\_ _ _ _ users evaluations -> csvTable users evaluations)
          (\_ _ _ _ -> noPreview)
          (\_ _ _ _ users evaluations -> csvTable users evaluations)
          pdata
        enablePreviewButton

  where
    titleLabel msg = msg . msg_NewAssessment_Title $ "Title"
    descriptionLabel msg = msg . msg_NewAssessment_Description $ "Description"

    formAction :: Pages.PageDesc -> Text -> H.Attribute
    formAction page encType = A.onclick (H.toValue $ T.concat ["javascript: form.action='", routeOf page, "'; form.enctype='", encType, "';"])
                              
    previewButton msg = Bootstrap.submitButtonWithAttr
                    (formAction preview "multipart/form-data" <> A.id "preview")
                    (msg . msg_NewAssessment_PreviewButton $ "Preview")
    downloadCsvButton msg = Bootstrap.blockButtonLink
                        getCsvLink
                        (msg . msg_NewAssessment_GetCsvButton $ "Get CSV")
    commitButton msg = Bootstrap.submitButtonWithAttrColorful
                   (formAction commit "multipart/form-data")
                   (msg . msg_NewAssessment_SaveButton $ "Commit")

    enablePreviewButton = H.script . H.toMarkup $ T.unwords
                            [ "document.getElementById('csv').onchange = function() {"
                            , "  document.getElementById('preview').disabled = false;"
                            , "};"
                            ]

    title, description :: Text
    (title,description) = pageDataAlgebra
                            (\_ -> ("",""))
                            (\_ -> ("",""))
                            (\_ title description _ _ _ -> (title,description))
                            (\_ title description _ _ _ -> (title,description))
                            (\_ as _ _ -> assessment (\title description _ _ _ -> (title,description)) as)
                            (\_ as _ _ _ _ -> assessment (\title description _ _ _ -> (title,description)) as)
                            pdata

    preview = pageDataAlgebra
                (\ck -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk -> Pages.fillNewGroupAssessmentPreview gk ())
                (\ck _ _ _ _ _ -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk _ _ _ _ _ -> Pages.fillNewGroupAssessmentPreview gk ())
                (\ak _ _ _ -> Pages.modifyAssessmentPreview ak ())
                (\ak _ _ _ _ _ -> Pages.modifyAssessmentPreview ak ())
                pdata
    getCsvLink = pageDataAlgebra
                 (\ck -> getEmptyCourseCsv ck)
                 (\gk -> getEmptyGroupCsv gk)
                 (\ck _ _ _ _ _ -> getEmptyCourseCsv ck)
                 (\gk _ _ _ _ _ -> getEmptyGroupCsv gk)
                 (\ak _ cGKey _ -> getFilledCsv ak cGKey)
                 (\ak _ cGKey _ _ _ -> getFilledCsv ak cGKey)
               pdata
        where
          getFilledCsv ak cGKey = either (getFilledCourseCsv ak) (getFilledGroupCsv ak) cGKey
          getFilledCourseCsv ak ck = routeWithOptionalParams (Pages.getCourseCsv ck ()) [requestParam ak]
          getFilledGroupCsv ak gk = routeWithOptionalParams (Pages.getGroupCsv gk ()) [requestParam ak]
          getEmptyCourseCsv ck = routeOf $ Pages.getCourseCsv ck ()
          getEmptyGroupCsv gk = routeOf $ Pages.getGroupCsv gk ()

    commit = pageDataAlgebra
               (\ck -> Pages.newCourseAssessment ck ())
               (\gk -> Pages.newGroupAssessment gk ())
               (\ck _ _ _ _ _ -> Pages.newCourseAssessment ck ())
               (\gk _ _ _ _ _ -> Pages.newGroupAssessment gk ())
               (\ak _ _ _ -> Pages.modifyAssessment ak ())
               (\ak _ _ _ _ _ -> Pages.modifyAssessment ak ())
               pdata

    readOnlyEvType = pageDataAlgebra
                       (\_ -> False)
                       (\_ -> False)
                       (\_ _ _ _ _ _ -> False)
                       (\_ _ _ _ _ _ -> False)
                       (\_ _ _ isScoreSubmitted -> isScoreSubmitted)
                       (\_ _ _ isScoreSubmitted _ _ -> isScoreSubmitted)
                       pdata

    showEvaluationType :: I18N -> EvConfig -> H.Html
    showEvaluationType msg eType =
      Bootstrap.formGroup $ do
        Bootstrap.readOnlyTextInputWithDefault ""
          (msg $ msg_NewAssessment_EvaluationType "Evaluation Type")
          (evConfigCata
            (msg $ msg_NewAssessment_BinaryEvaluation "Binary")
            (const . msg $ msg_NewAssessment_PercentageEvaluation "Percentage")
            (msg $ msg_NewAssessment_FreeFormEvaluation "Free form textual")
            eType)
        let evConfigName = "evConfig" :: Text
        hiddenInput evConfigName (Bootstrap.encode "Evaluation type" eType)
        H.toMarkup . msg $ msg_NewAssessment_EvalTypeWarn "The evaluation type can not be modified, there is a score for the assessment."

    selectedEvType = pageDataAlgebra
                       (\_ -> defaultEvType)
                       (\_ -> defaultEvType)
                       (\_ _ _ evConfig _ _ -> evConfig)
                       (\_ _ _ evConfig _ _ -> evConfig)
                       (\_ as _ _ -> evaluationCfg as)
                       (\_ as _ _ _ _ -> evaluationCfg as)
                       pdata
                           where defaultEvType = binaryConfig

previewTable :: I18N -> [UserDesc] -> M.Map UserDesc Evaluation -> H.Html
previewTable msg users evaluations = Bootstrap.table $ do
  header
  tableData
    where 
      header = H.tr $ H.th studentName >> H.th username >> H.th score
          where 
            studentName = H.toMarkup . msg . msg_NewAssessment_StudentName $ "Name"
            username = H.toMarkup . msg . msg_NewAssessment_UserName $ "Username"
            score = H.toMarkup . msg . msg_NewAssessment_Score $ "Score"
             
      tableData :: H.Html
      tableData = mapM_ tableRow (sortBy (compareHun `on` ud_fullname) users)

      tableRow :: UserDesc -> H.Html
      tableRow user = H.tr $ do
        H.td $ H.toMarkup fullname
        H.td $ H.toMarkup user_uid
        H.td $ formatEvResultMaybe toLargeIcon msg (evaluationResult <$> M.lookup user evaluations)
          where fullname = ud_fullname user
                user_uid = uid id . ud_uid $ user

readCsv :: B.ByteString -> M.Map Uid String
readCsv bs = foldr (f . dropSpaces) M.empty (B.lines bs)
    where
      f line m | B.null line        = m
               | B.head line == '#' = m
               | otherwise          = if (not (null score'))
                                      then M.insert username' score' m
                                      else m
          where
            (_fullname,unameAndScore) = B.break (== ',') line
 
            (username,score) = case B.uncons unameAndScore of
                                 Just (',',bs) -> B.break (== ',') bs
                                 _             -> ("","")
 
            score' = case B.uncons score of
                       Just (',',cs) -> BsUTF8.toString cs
                       _             -> ""
 
            username' = Uid . B.unpack . stripSpaces $ username

      dropSpaces = B.dropWhile isSpace
      stripSpaces = B.reverse . dropSpaces . B.reverse . dropSpaces

viewAssessmentPage :: GETContentHandler
viewAssessmentPage = do
  ak <- getParameter assessmentKeyPrm
  aDesc <- userStory $ Story.assessmentDesc ak
  setPageContents $ htmlPage (msg_LinkText_ViewAssessment "View Assessment") $
    viewAssessmentContent aDesc

viewAssessmentContent :: AssessmentDesc -> IHtml
viewAssessmentContent aDesc = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 . Bootstrap.table . H.tbody $ do
      (msg . msg_ViewAssessment_Course $ "Course:")   .|. adCourse aDesc
      maybe mempty (\g -> (msg . msg_ViewAssessment_Group $ "Group:") .|. g) (adGroup aDesc)
      (msg . msg_ViewAssessment_Teacher $ "Teacher:") .|. (intercalate ", " . sortHun . adTeacher) aDesc
      (msg . msg_ViewAssessment_Assessment $ "Assessment:") .|. title
      when (not . T.null $ description) $
        (msg . msg_ViewAssessment_Description $ "Description:") .|. description
    where
      title, description :: Text
      (title, description) = let assessment = adAssessment aDesc
                             in withAssessment assessment (\title description _ _ _ -> (title, description))

evTypeSelection :: I18N -> EvConfig -> H.Html
evTypeSelection msg selected = Bootstrap.selectionWithLabel "evConfig" evalType (== selected) selection 
    where selection = [ (binaryConfig, binary)
                      , (percentageConfig 0.0, percentage)
                      , (freeFormConfig, freeForm)
                      ]
          evalType = msg . msg_NewAssessment_EvaluationType $ "Evaluation Type"
          binary = msg . msg_NewAssessment_BinaryEvaluation $ "Binary"
          percentage = msg . msg_NewAssessment_PercentageEvaluation $ "Percentage"
          freeForm = msg . msg_NewAssessment_FreeFormEvaluation $ "Free form textual"

modifyAssessmentPage :: GETContentHandler
modifyAssessmentPage = do
  ak <- getParameter assessmentKeyPrm
  (as,cGKey,scoreSubmitted) <- userStory $ do
    as <- Story.loadAssessment ak
    scoreSubmitted <- Story.isThereAScore ak
    cGKey <- Story.courseOrGroupOfAssessment ak
    return (as,cGKey,scoreSubmitted)
  setPageContents $ htmlPage (msg_LinkText_ModifyAssessment "Modify Assessment") $
    fillAssessmentTemplate $ PD_ModifyAssessment ak as cGKey scoreSubmitted

postModifyAssessment :: POSTContentHandler
postModifyAssessment = do
  msg <- i18nE
  uploadResult <- uploadFile
  ak <- getParameter assessmentKeyPrm
  newTitle <- getParameter titleParam
  newDesc <- getParameter descriptionParam
  selectedEvType <- getParameter evConfigParam
  now <- liftIO getCurrentTime
  ckGk <- userStory $ Story.courseOrGroupOfAssessment ak
  let a = Assessment {
            title         = newTitle
          , description   = newDesc
          , evaluationCfg = selectedEvType
          , created       = now
          , visible       = True
          }
      redirectionTarget = either
        (\ck -> Pages.courseManagement ck Pages.AssignmentsContents ())
        (\gk -> Pages.groupOverview gk ())
        ckGk
  case uploadResult of
    [File _name contents] -> do
      users <- userStory $ do
        cGKey <- Story.courseOrGroupOfAssessment ak
        usernames <- either Story.subscribedToCourse Story.subscribedToGroup cGKey
        mapM Story.loadUserDesc usernames
      let evaluations = fromUserDescKey (toUserDescKey ud_uid users (parseEvaluations msg selectedEvType (readCsv contents)))
      return $ Action $ do
        Story.modifyAssessmentAndScores ak a evaluations
        return $ redirection redirectionTarget
    _ -> do
      evaluations <- read <$> getParameter evaluationsParam
      return $ Action $ do
        if M.null evaluations
          then Story.modifyAssessment ak a
          else Story.modifyAssessmentAndScores ak a evaluations
        return $ redirection redirectionTarget
  
modifyAssessmentPreviewPage :: ViewPOSTContentHandler
modifyAssessmentPreviewPage = do
  msg <- i18nE
  uploadResult <- uploadFile
  ak <- getParameter assessmentKeyPrm
  selectedEvType <- getParameter evConfigParam
  (as,cGKey,scoreSubmitted,users) <- userStory $ do
    as <- Story.loadAssessment ak
    scoreSubmitted <- Story.isThereAScore ak
    cGKey <- Story.courseOrGroupOfAssessment ak
    usernames <- either Story.subscribedToCourse Story.subscribedToGroup cGKey
    users <- mapM Story.loadUserDesc usernames
    return (as,cGKey,scoreSubmitted,users)
  let [File _name contents] = uploadResult
      evaluations = toUserDescKey ud_uid users (parseEvaluations msg selectedEvType (readCsv contents))
  setPageContents $ htmlPage (msg_LinkText_ModifyAssessmentPreview "Modify Assessment") $
    fillAssessmentTemplate $ PD_ModifyAssessmentPreview ak as cGKey scoreSubmitted users evaluations
