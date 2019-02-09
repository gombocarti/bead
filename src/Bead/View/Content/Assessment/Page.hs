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

import           Bead.View.Content hiding (getForm, postForm)
import           Bead.View.Content.Bootstrap ((.|.), FixedChoiceInput)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.View.Content.Form as Form
import           Bead.View.Content.ScoreInfo (scoreInfoToIcon)
import           Bead.View.RequestParams
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import qualified Bead.Domain.Entity.Assessment as A
import           Bead.Domain.Evaluation
import qualified Bead.Domain.JSON as JSON

import           Data.Char (toUpper,isSpace)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BsUTF8 (toString)
import           Data.Function (on)
import qualified Data.Map.Strict as M
import           Data.Maybe (listToMaybe, maybe, fromMaybe)
import           Data.List (sortBy,intercalate)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import           Data.Time (UTCTime, getCurrentTime)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!))
import qualified Text.Digestive as DF
import           Text.Digestive ((.:))
import           Text.Read (readMaybe)
import           Control.Applicative ((<|>))
import           Control.Monad (join,when)
import           Control.Monad.Identity (Identity(Identity), runIdentity)
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

type Title = String
type Description = String

data PageData = PD_NewCourseAssessment CourseKey Form.View
              | PD_NewGroupAssessment GroupKey Form.View
              | PD_PreviewCourseAssessment CourseKey (Maybe ([UserDesc], M.Map Uid Evaluation)) Form.View
              | PD_PreviewGroupAssessment GroupKey (Maybe ([UserDesc], M.Map Uid Evaluation)) Form.View
              | PD_ModifyAssessment {
                  pdAKey             :: AssessmentKey
                , pdAs               :: Assessment
                , pdCourseOrGroupKey :: Either CourseKey GroupKey
                , pdIsScoreSubmitted :: Bool
                , pdFormContents     :: Form.View
                }
              | PD_ModifyAssessmentPreview {
                  pdAKey             :: AssessmentKey
                , pdAs               :: Assessment
                , pdCourseOrGroupKey :: Either CourseKey GroupKey
                , pdIsScoreSubmitted :: Bool
                , pdEvaluations      :: Maybe ([UserDesc], M.Map Uid Evaluation)
                , pdFormContents     :: Form.View
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
        PD_NewCourseAssessment ck formContents ->
          newCourseAssessment ck formContents
        PD_NewGroupAssessment gk formContents ->
          newGroupAssessment gk formContents
        PD_PreviewCourseAssessment ck evaluations formContents ->
          previewCourseAssessment ck evaluations formContents
        PD_PreviewGroupAssessment gk evaluations formContents ->
          previewGroupAssessment gk evaluations formContents
        PD_ModifyAssessment ak as cGKey isScoreSubmitted formContents ->
          modifyAssessment ak as cGKey isScoreSubmitted formContents
        PD_ModifyAssessmentPreview ak as cGKey isScoreSubmitted evaluations formContents ->
          modifyAssessmentPreview ak as cGKey isScoreSubmitted evaluations formContents

data EvConfigAccess = ReadOnly | ReadWrite

evAccessCata :: a -> a -> EvConfigAccess -> a
evAccessCata readOnly readWrite evAccess =
  case evAccess of
    ReadOnly -> readOnly
    ReadWrite -> readWrite

data FormData = FormData {
    fdTitle           :: String
  , fdDesc            :: String
  , fdEvConfig        :: EvConfig
  , fdPrevEvaluations :: T.Text
  , fdNewEvaluations  :: M.Map Uid Evaluation
  }

formDataCata f (FormData title desc evConfig prevEvaluations newEvaluations) =
  f title desc evConfig prevEvaluations newEvaluations

newGroupAssessmentPage :: GETContentHandler
newGroupAssessmentPage = do
  msg <- i18nE
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  let formContents = getForm (form msg Nothing)
  setPageContents $ fillAssessmentTemplate $ PD_NewGroupAssessment gk formContents

postNewGroupAssessment :: POSTContentHandler
postNewGroupAssessment = do 
  msg <- i18nE
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  res <- postForm (form msg Nothing) Form.InsertFilesIntoView
  case res of
    (_, Just formData) -> do
      now <- liftIO getCurrentTime
      users <- usersInGroup gk
      let assessment = formToAssessment now formData
          evaluations = toUsernameKey (formToEvaluations msg users formData)
      setUserAction $ if M.null evaluations
                      then CreateGroupAssessment gk assessment
                      else SaveScoresOfGroupAssessment gk assessment evaluations
    (errors, _) -> do
      setErrorContents $ fillAssessmentTemplate $ PD_NewGroupAssessment gk errors

assessmentToForm :: Assessment -> FormData
assessmentToForm = A.assessment (\title desc _creation evConfig _visible ->
                                    FormData title desc evConfig "" M.empty)

formToAssessment :: UTCTime -> FormData -> Assessment
formToAssessment now = formDataCata (\title desc evConfig _prevEvaluations _newEvaluations ->
                                       Assessment title desc now evConfig visible)
  where
    visible :: Bool
    visible = True

formToEvaluations :: I18N -> [UserDesc] -> FormData -> M.Map UserDesc Evaluation
formToEvaluations msg users = formDataCata (\_title _desc evConfig prevEvaluations newEvaluations ->
                                              let prev = case JSON.safeDecodeJSON prevEvaluations of
                                                           JSON.Ok evaluations -> evaluationsOf users evaluations
                                                           _                   -> M.empty
                                                  new = evaluationsOf users newEvaluations
                                              in if not $ M.null new
                                                 then new
                                                 else prev
                                           )
  where
    evaluationsOf :: [UserDesc] -> M.Map Uid a -> M.Map UserDesc a
    evaluationsOf users m = foldr f M.empty users
      where 
        f user acc = maybe acc (\a -> M.insert user a acc) (M.lookup (ud_uid user) m)

toUsernameKey :: M.Map UserDesc a -> M.Map Username a
toUsernameKey = M.mapKeys ud_username

toUidKey :: M.Map UserDesc a -> M.Map Uid a
toUidKey = M.mapKeys ud_uid

usersInGroup :: GroupKey -> ContentHandler [UserDesc]
usersInGroup gk = userStory $ do
  usernames <- Story.subscribedToGroup gk
  mapM Story.loadUserDesc usernames

usersInCourse :: CourseKey -> ContentHandler [UserDesc]
usersInCourse ck = userStory $ do
  usernames <- Story.subscribedToCourse ck
  mapM Story.loadUserDesc usernames

newCourseAssessmentPage :: GETContentHandler
newCourseAssessmentPage = do
  msg <- i18nE
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  let formContents = getForm (form msg Nothing)
  setPageContents $ fillAssessmentTemplate $ PD_NewCourseAssessment ck formContents

postNewCourseAssessment :: POSTContentHandler
postNewCourseAssessment = do 
  msg <- i18nE
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  res <- postForm (form msg Nothing) Form.InsertFilesIntoView
  case res of
    (_, Just formData) -> do
      now <- liftIO getCurrentTime
      users <- usersInCourse ck
      let assessment = formToAssessment now formData
          evaluations = toUsernameKey (formToEvaluations msg users formData)
      setUserAction $ if M.null evaluations
                      then CreateCourseAssessment ck assessment
                      else SaveScoresOfCourseAssessment ck assessment evaluations
    (errors, _) -> do
      setErrorContents $ fillAssessmentTemplate $ PD_NewCourseAssessment ck errors

parseEvaluations :: I18N -> EvConfig -> M.Map Uid T.Text -> M.Map Uid Evaluation
parseEvaluations msg evConfig = M.mapMaybe (parseEvaluation msg evConfig)

parseEvaluation :: I18N -> EvConfig -> T.Text -> Maybe Evaluation
parseEvaluation _msg _evConfig "" = Nothing
parseEvaluation  msg  evConfig  s = mkEval <$>
                                    evConfigCata
                                      readBinary
                                      (const readPercentage)
                                      (Just . freeFormResult . T.unpack $ s)
                                      evConfig
    where mkEval :: EvResult -> Evaluation
          mkEval result = Evaluation result ""

          readBinary :: Maybe EvResult
          readBinary | isAccepted = Just . binaryResult $ Passed
                     | isRejected = Just . binaryResult $ Failed
                     | otherwise  = Nothing
              where
                isAccepted = normalized `elem` [accepted,"+","1"]
                isRejected = normalized `elem` [rejected,"-","0"]
                normalized = T.toUpper (T.strip s)
                accepted   = T.pack $ map toUpper (msg . msg_NewAssessment_Accepted $ "Accepted")
                rejected   = T.pack $ map toUpper (msg . msg_NewAssessment_Rejected $ "Rejected")

          readPercentage :: Maybe EvResult
          readPercentage = case TR.decimal s of
                             Right (n, _) ->
                               if n >= 0 && n <= 100
                               then Just . percentageResult . (/100) . fromIntegral $ n
                               else Nothing
                             Left _ -> Nothing

fillNewGroupAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewGroupAssessmentPreviewPage = do
  msg <- i18nE
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  postResult <- postForm (form msg Nothing) Form.InsertFilesIntoView
  case postResult of
    (v, Just formData) -> do
      users <- usersInGroup gk
      let evaluations = toUidKey (formToEvaluations msg users formData)
      setPageContents $ fillAssessmentTemplate $ PD_PreviewGroupAssessment gk (Just (users, evaluations)) v
    (errors, _) ->
      setPageContents $ fillAssessmentTemplate $ PD_PreviewGroupAssessment gk Nothing errors

fillNewCourseAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewCourseAssessmentPreviewPage = error "fillNewCourseAssessmentPreviewPage is undefined"

assTitle :: T.Text
assTitle = "title"

assDesc :: T.Text
assDesc = "desc"

assEvConfig :: T.Text
assEvConfig = "evConfig"

assPrevEvaluations :: T.Text
assPrevEvaluations = "evaluations_prev"

assNewEvaluations :: T.Text
assNewEvaluations = "evaluations_new"

formName :: T.Text
formName = "assessment"

getForm :: Form.Form a -> Form.View
getForm = Form.getForm formName

postForm :: Form.Form a -> Form.UploadPolicy -> ContentHandler (Form.View, Maybe a)
postForm = Form.postForm formName

form :: I18N -> Maybe FormData -> Form.Form FormData
form msg defaults = formData <$>
  assTitle .: DF.check (T.pack $ msg $ msg_Assessment_CannotBeEmpty "Cannot be empty.") (not . null) (DF.string (fdTitle <$> defaults))
  <*>
  assDesc .: DF.string (fdDesc <$> defaults)
  <*>
  assEvConfig .: (maybe id (\ev -> DF.check (T.pack $ msg $ msg_Assessment_CannotChange "Cannot change.") (== ev)) (fdEvConfig <$> defaults) $ evConfig (fdEvConfig <$> defaults))
  <*>
  assPrevEvaluations .: DF.text (fdPrevEvaluations <$> defaults)
  <*>
  assNewEvaluations .: DF.text Nothing
  where
    formData :: String -> String -> EvConfig -> Text -> Text -> FormData
    formData title desc evConfig prevEvaluations newEvaluations = FormData {
        fdTitle           = title
      , fdDesc            = desc
      , fdEvConfig        = evConfig
      , fdPrevEvaluations = prevEvaluations
      , fdNewEvaluations  = parseEvaluations msg evConfig (readCsv newEvaluations)
      }

    evConfig :: Maybe EvConfig -> Form.Form EvConfig
    evConfig = DF.choice options
      where
        options :: [(EvConfig, T.Text)]
        options = [ (binaryConfig, binary)
                  , (percentageConfig 0, percentage)
                  , (freeFormConfig, freeForm)
                  ]

        binary = T.pack $ msg $ msg_NewAssessment_BinaryEvaluation $ "Binary"
        percentage = T.pack $ msg $ msg_NewAssessment_PercentageEvaluation $ "Percentage"
        freeForm = T.pack $ msg $ msg_NewAssessment_FreeFormEvaluation $ "Free form textual"

    evaluations :: String -> Maybe (M.Map Uid T.Text) -> Form.Form (M.Map Uid T.Text)
    evaluations v evals = DF.validate (DF.Success . readCsv) (DF.text ((T.pack . show) <$> evals))

view :: I18N -> PageData -> EvConfigAccess -> Form.View -> H.Html
view msg pdata evAccess v = H.form ! A.method "post" ! A.enctype "multipart/form-data" $ do
  Bootstrap.formGroup
    (T.pack . msg . msg_NewAssessment_Title $ "Title")
    (Bootstrap.with (Form.textInput assTitle v) [Bootstrap.required])
    (Form.errorOf assTitle v)
  Bootstrap.formGroup
    (T.pack . msg . msg_NewAssessment_Description $ "Description")
    (Form.textInput assDesc v)
    (Form.errorOf assDesc v)
  evConfig msg
  hiddenInput (T.unpack $ DF.absoluteRef assPrevEvaluations v) (T.unpack $ JSON.encodeJSON evaluations)
  Bootstrap.formGroup
    "Csv"
    (Form.file assNewEvaluations v)
    Nothing
  Bootstrap.row $ do
    Bootstrap.colMd4 (previewButton msg ! A.disabled "")
    Bootstrap.colMd4 (downloadCsvButton msg)
    Bootstrap.colMd4 (commitButton msg)
  enablePreviewButton

  where
    evConfig :: I18N -> H.Html
    evConfig msg = evAccessCata readonly readwrite evAccess
      where
        readonly :: H.Html
        readonly = do
          Bootstrap.formGroup
            evalType
            (Bootstrap.with selection [Bootstrap.disabled])
            Nothing
          fromString . msg $ msg_NewAssessment_EvalTypeWarn "The evaluation type can not be modified, there is a score for the assessment."

        readwrite :: H.Html
        readwrite = Bootstrap.formGroup evalType selection Nothing

        selection :: FixedChoiceInput
        selection = Form.selection assEvConfig v

        evalType :: Text
        evalType = T.pack . msg . msg_NewAssessment_EvaluationType $ "Evaluation Type"

    evaluations :: M.Map Uid Evaluation
    evaluations =
      pageDataAlgebra
        (\_ _ -> M.empty)
        (\_ _ -> M.empty)
        (\_ evs _ -> maybe M.empty snd evs)
        (\_ evs _ -> maybe M.empty snd evs)
        (\_ _ _ _ _ -> M.empty)
        (\_ _ _ _ evs _ -> maybe M.empty snd evs)
        pdata

    title msg = msg . msg_NewAssessment_Title $ "Title"
    description msg = msg . msg_NewAssessment_Description $ "Description"

    previewId :: T.Text
    previewId = "preview"

    previewButton msg =
      Bootstrap.submitButtonWithAttr
        (formAction preview <> A.id (H.toValue previewId))
        (msg . msg_NewAssessment_PreviewButton $ "Preview")

    downloadCsvButton msg =
      Bootstrap.blockButtonLink
        getCsvLink
        (msg . msg_NewAssessment_GetCsvButton $ "Get CSV")
      
    commitButton msg =
      Bootstrap.submitButtonWithAttrColorful
        (formAction commit)
        (msg . msg_NewAssessment_SaveButton $ "Commit")

    enablePreviewButton =
      H.script . H.toMarkup $ T.concat
        [ "document.getElementById('" , DF.absoluteRef assNewEvaluations v, "').onchange = function() {"
        , "  document.getElementById('" , previewId, "').disabled = false;"
        , "};"
        ]

    preview = pageDataAlgebra
                (\ck _ -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk _ -> Pages.fillNewGroupAssessmentPreview gk ())
                (\ck _ _ -> Pages.fillNewCourseAssessmentPreview ck ())
                (\gk _ _ -> Pages.fillNewGroupAssessmentPreview gk ())
                (\ak _ _ _ _ -> Pages.modifyAssessmentPreview ak ())
                (\ak _ _ _ _ _ -> Pages.modifyAssessmentPreview ak ())
                pdata

    getCsvLink = pageDataAlgebra
                   (\ck _ -> getEmptyCourseCsv ck)
                   (\gk _ -> getEmptyGroupCsv gk)
                   (\ck _ _ -> getEmptyCourseCsv ck)
                   (\gk _ _ -> getEmptyGroupCsv gk)
                   (\ak _ cGKey _ _ -> getFilledCsv ak cGKey)
                   (\ak _ cGKey _ _ _ -> getFilledCsv ak cGKey)
                   pdata
        where
          getFilledCsv ak cGKey = either (getFilledCourseCsv ak) (getFilledGroupCsv ak) cGKey
          getFilledCourseCsv ak ck = routeWithOptionalParams (Pages.getCourseCsv ck ()) [requestParam ak]
          getFilledGroupCsv ak gk = routeWithOptionalParams (Pages.getGroupCsv gk ()) [requestParam ak]
          getEmptyCourseCsv ck = routeOf $ Pages.getCourseCsv ck ()
          getEmptyGroupCsv gk = routeOf $ Pages.getGroupCsv gk ()

    commit = pageDataAlgebra
               (\ck _ -> Pages.newCourseAssessment ck ())
               (\gk _ -> Pages.newGroupAssessment gk ())
               (\ck _ _ -> Pages.newCourseAssessment ck ())
               (\gk _ _ -> Pages.newGroupAssessment gk ())
               (\ak _ _ _ _ -> Pages.modifyAssessment ak ())
               (\ak _ _ _ _ _ -> Pages.modifyAssessment ak ())
               pdata

    formAction :: Pages.PageDesc -> H.Attribute
    formAction page = A.onclick (fromString $ concat ["javascript: form.action='", routeOf page, "';"])

fillAssessmentTemplate :: PageData -> IHtml
fillAssessmentTemplate pdata = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do
      view msg pdata evConfigAccess formContents
      previewCsv msg
  where
    formContents :: Form.View
    formContents = pageDataAlgebra
                 (\_ v -> v)
                 (\_ v -> v)
                 (\_ _ v -> v)
                 (\_ _ v -> v)
                 (\_ _ _ _ v -> v)
                 (\_ _ _ _ _ v -> v)
                 pdata

    previewCsv :: I18N -> H.Html
    previewCsv msg = 
      pageDataAlgebra
        (\_ _ -> noPreview)
        (\_ _ -> noPreview)
        (\_ evs _ -> maybe noPreview (uncurry $ previewTable msg) evs)
        (\_ evs _ -> maybe noPreview (uncurry $ previewTable msg) evs)
        (\_ _ _ _ _ -> noPreview)
        (\_ _ _ _ evs _ -> maybe noPreview (uncurry $ previewTable msg) evs)
        pdata

    noPreview :: H.Html
    noPreview = mempty

    evConfigAccess :: EvConfigAccess
    evConfigAccess = pageDataAlgebra
                       (\_ _ -> ReadWrite)
                       (\_ _ -> ReadWrite)
                       (\_ _ _ -> ReadWrite)
                       (\_ _ _ -> ReadWrite)
                       (\_ _ _ isScoreSubmitted _ -> if isScoreSubmitted then ReadOnly else ReadWrite)
                       (\_ _ _ isScoreSubmitted _ _ -> if isScoreSubmitted then ReadOnly else ReadWrite)
                       pdata

previewTable :: I18N -> [UserDesc] -> M.Map Uid Evaluation -> H.Html
previewTable msg users evaluations = Bootstrap.table $ do
  header
  tableData
    where 
      header = H.tr $ H.th studentName >> H.th username >> H.th score
          where 
            studentName = fromString . msg . msg_NewAssessment_StudentName $ "Name"
            username = fromString . msg . msg_NewAssessment_UserName $ "Username"
            score = fromString . msg . msg_NewAssessment_Score $ "Score"
             
      tableData :: H.Html
      tableData = mapM_ tableRow (sortBy (compareHu `on` ud_fullname) users)

      tableRow :: UserDesc -> H.Html
      tableRow user = H.tr $ do
        H.td $ fromString fullname
        H.td $ fromString (uid id user_uid)
        H.td $ case M.lookup user_uid evaluations of
                 Just evaluation -> let scoreInfo = evaluationCata (\result _comment -> (Score_Result (EvaluationKey "") result)) evaluation in
                                    scoreInfoToIcon msg scoreInfo
                 Nothing         -> scoreInfoToIcon msg Score_Not_Found

          where fullname = ud_fullname user
                user_uid = ud_uid user

readCsv :: T.Text -> M.Map Uid T.Text
readCsv bs = foldr (f . T.stripStart) M.empty (T.lines bs)
    where
      f line m | T.null line        = m
               | T.head line == '#' = m
               | otherwise          =
                   case T.splitOn "," line of
                     _fullname : username : score : _ | not (T.null score) ->
                       let username' = Uid . T.unpack . T.strip $ username
                       in M.insert username' score m
                     _ -> m

viewAssessmentPage :: GETContentHandler
viewAssessmentPage = do
  ak <- getParameter assessmentKeyPrm
  aDesc <- userStory $ Story.assessmentDesc ak
  setPageContents $ viewAssessmentContent aDesc

viewAssessmentContent :: AssessmentDesc -> IHtml
viewAssessmentContent aDesc = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 . Bootstrap.table . H.tbody $ do
      (msg . msg_ViewAssessment_Course $ "Course:")   .|. adCourse aDesc
      maybe mempty (\g -> (msg . msg_ViewAssessment_Group $ "Group:") .|. g) (adGroup aDesc)
      (msg . msg_ViewAssessment_Teacher $ "Teacher:") .|. (intercalate ", " . sortHu . adTeacher) aDesc
      (msg . msg_ViewAssessment_Assessment $ "Assessment:") .|. title
      when (not . null $ description) $
        (msg . msg_ViewAssessment_Description $ "Description:") .|. description
    where
      title, description :: String
      (title, description) = let assessment = adAssessment aDesc
                             in withAssessment assessment (\title description _ _ _ -> (title, description))

modifyAssessmentPage :: GETContentHandler
modifyAssessmentPage = do
  msg <- i18nE
  ak <- getParameter assessmentKeyPrm
  (a,cGKey,scoreSubmitted) <- userStory $ do
    a <- Story.loadAssessment ak
    scoreSubmitted <- Story.isThereAScore ak
    cGKey <- Story.courseOrGroupOfAssessment ak
    return (a, cGKey, scoreSubmitted)
  let formContents = getForm (form msg (Just $ assessmentToForm a))
  setPageContents . fillAssessmentTemplate $ PD_ModifyAssessment ak a cGKey scoreSubmitted formContents

postModifyAssessment :: POSTContentHandler
postModifyAssessment = do
  msg <- i18nE
  ak <- getParameter assessmentKeyPrm
  a <- userStory $ Story.loadAssessment ak
  res <- postForm (form msg (Just $ assessmentToForm a)) Form.InsertFilesIntoView
  case res of
    (_, Just formData) -> do
      cGKey <- userStory $ Story.courseOrGroupOfAssessment ak
      users <- either usersInCourse usersInGroup cGKey
      let a' = formToAssessment (A.created a) formData
          evaluations = toUsernameKey (formToEvaluations msg users formData)
      setUserAction $ if M.null evaluations
                      then ModifyAssessment ak a'
                      else ModifyAssessmentAndScores ak a' evaluations
    (errors, _) -> do
      (cGKey, scoreSubmitted) <- userStory $ do
        cGKey <- Story.courseOrGroupOfAssessment ak
        (,) <$> pure cGKey <*> Story.isThereAScore ak
      setErrorContents . fillAssessmentTemplate $ PD_ModifyAssessment ak a cGKey scoreSubmitted errors

modifyAssessmentPreviewPage :: ViewPOSTContentHandler
modifyAssessmentPreviewPage = do
  msg <- i18nE
  ak <- getParameter assessmentKeyPrm
  (a,cGKey,scoreSubmitted,users) <- userStory $ do
    a <- Story.loadAssessment ak
    scoreSubmitted <- Story.isThereAScore ak
    cGKey <- Story.courseOrGroupOfAssessment ak
    usernames <- either Story.subscribedToCourse Story.subscribedToGroup cGKey
    users <- mapM Story.loadUserDesc usernames
    return (a, cGKey, scoreSubmitted, users)
  res <- postForm (form msg (Just $ assessmentToForm a)) Form.InsertFilesIntoView
  let page = case res of
               (v, Just formData) ->
                 PD_ModifyAssessmentPreview
                   ak
                   (formToAssessment (A.created a) formData)
                   cGKey
                   scoreSubmitted
                   (Just (users, toUidKey (formToEvaluations msg users formData)))
                   v
               (errors, _) ->
                 PD_ModifyAssessmentPreview
                   ak
                   a
                   cGKey
                   scoreSubmitted
                   Nothing
                   errors
  setPageContents . fillAssessmentTemplate $ page

