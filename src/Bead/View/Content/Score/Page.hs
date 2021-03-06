{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Score.Page (
    viewUserScore
  , newUserScore
  , modifyUserScore
  ) where

import           Bead.View.Content
import           Bead.View.Content.Bootstrap ((.|.))
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.StateVisualization (formatEvResult, formatEvResultMaybe, toPlainText)
import           Bead.View.Content.VisualConstants
import           Bead.View.RequestParams
import           Bead.Controller.Pages (PageDesc)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.Domain.Shared.Evaluation hiding (evConfig)

import qualified Text.Blaze as B
import           Text.Blaze.Html5 (Html,Attribute,(!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Printf
import           Control.Monad (when)
import           Data.Either (either)
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

data PageData
  = PD_NewUserScore {
      pdStudent        :: String
    , pdUsername       :: Username
    , pdUid            :: Uid
    , pdAssessmentDesc :: AssessmentDesc
    }      
  | PD_ModifyUserScore {
      pdStudent        :: String
    , pdUsername       :: Username
    , pdUid            :: Uid
    , pdAssessmentDesc :: AssessmentDesc
    , pdScore          :: ScoreInfo
    }

pageDataAlgebra
  newUserScore_
  modifyUserScore_
  pd = case pd of
         PD_NewUserScore student uname uid assessmentDesc -> newUserScore_ student uname uid assessmentDesc
         PD_ModifyUserScore student uname uid assessmentDesc score -> modifyUserScore_ student uname uid assessmentDesc score
    
newUserScore :: ViewModifyHandler
newUserScore = ViewModifyHandler scorePage newScorePostHandler

modifyUserScore :: ViewModifyHandler
modifyUserScore = ViewModifyHandler modifyScorePage modifyScorePostHandler

viewUserScore :: ViewHandler
viewUserScore = ViewHandler viewScorePage

scorePage :: GETContentHandler
scorePage = do 
  username <- getParameter $ usernamePrm
  ak <- getParameter $ assessmentKeyPrm
  user <- userStory (Story.loadUser username)
  let uname = u_name user
      uid = u_uid user
  assessmentDesc <- userStory (Story.assessmentDesc ak)
  setPageContents $ htmlPage (msg_LinkText_NewUserScore "New Score") $
    scoreContent $ PD_NewUserScore uname username uid assessmentDesc

newScorePostHandler :: POSTContentHandler
newScorePostHandler = do
  username <- getParameter $ usernamePrm
  ak <- getParameter $ assessmentKeyPrm
  evConfig <- evConfig <$> userStory (Story.loadAssessment ak)
  evaluation <- getEvaluation evConfig
  let success ev = Action $ do
        sk <- Story.saveUserScore username ak ev
        return $ redirection $ Pages.modifyUserScore sk ()

      failure message = Action $ do
        Story.putErrorMessage message
        return $ redirection $ Pages.newUserScore ak username ()

  return $ either failure success evaluation

getEvaluation :: EvConfig -> ContentHandler (Either Translation Evaluation)
getEvaluation evConfig = do
  commentOrResult <-
    evConfigCata
    (getJSONParam (fieldName evaluationResultField) "No evaluation can be found.")
    (\_ -> do
      percentage  <- getParameter evaluationPercentagePrm
      return . EvCmtResult $ percentageResult (fromIntegral percentage / 100))
    (do freeForm <- getParameter freeFormEvaluationParam
        return . EvCmtResult $ freeFormResult $ T.pack freeForm)
    evConfig
  withEvalOrComment commentOrResult
    (return . Left $ msg_Evaluation_EmptyCommentAndFreeFormResult "Comments are not supported yet.")
    (\result -> return . Right $ Evaluation {
          evaluationResult = result
        , writtenEvaluation = ""
        })

modifyScorePage :: GETContentHandler
modifyScorePage = do
  sk <- getParameter scoreKeyPrm
  mScore <- userStory $ Story.scoreInfo sk
  case mScore of
    Nothing -> redirectTo $ Pages.modifyUserScore sk ()
    Just score -> do
      (user,username,assessmentDesc) <- userStory $ do
        username <- Story.usernameOfScore sk
        user <- Story.loadUser username
        ak <- Story.assessmentOfScore sk
        assessmentDesc <- Story.assessmentDesc ak
        return (user,username,assessmentDesc)
      let uname = u_name user
          uid = u_uid user
      setPageContents $ htmlPage (msg_LinkText_ModifyUserScore "Modify Score") $
        scoreContent $ PD_ModifyUserScore uname username uid assessmentDesc score

modifyScorePostHandler :: POSTContentHandler
modifyScorePostHandler = do
  sk <- getParameter scoreKeyPrm
  evConfig <- evConfig <$> userStory (do
                           ak <- Story.assessmentOfScore sk
                           Story.loadAssessment ak)
  evaluation <- getEvaluation evConfig
  let success ev = Action $ do
        Story.modifyUserScore sk ev
        return $ redirection $ Pages.modifyUserScore sk ()

      failure message = Action $ do
        Story.putErrorMessage message
        return $ redirection $ Pages.modifyUserScore sk ()

  return $ either failure success evaluation

scoreContent :: PageData -> IHtml
scoreContent pd = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 . Bootstrap.table . H.tbody $ do
      (msg . msg_NewUserScore_Course $ "Course:" )    .|. aCourse
      (msg . msg_NewUserScore_Assessment $ "Assessment:") .|. aTitle
      when (not . T.null $ aDesc) $ (msg . msg_NewUserScore_Description $ "Description:") .|. aDesc
      maybe mempty (\g -> (msg . msg_NewUserScore_Group $ "Group:") .|. g) aGroup
      (msg . msg_NewUserScore_Student $ "Student:")    .|. pdStudent pd
      (msg . msg_NewUserScore_UserName $ "Username:")   .|. (uid id $ pdUid pd)
    postForm (routeOf handler) $ do
      view msg
      evaluationInput msg
      submit msg
    where
      aCourse :: Text
      aCourse = adCourse . pdAssessmentDesc $ pd

      aGroup :: Maybe Text
      aGroup = adGroup . pdAssessmentDesc $ pd

      aTitle,aDesc :: Text
      (aTitle,aDesc) = assessment (\title desc _creation _cfg _visible -> (title,desc)) as

      as :: Assessment
      as = adAssessment . pdAssessmentDesc $ pd

      submit :: I18N -> Html
      submit msg = Bootstrap.submitButtonWithAttr mempty (msg . msg_NewUserScore_Submit $ "Submit")

      handler = pageDataAlgebra
                  (\_student uname _uid aDesc -> Pages.newUserScore (adAssessmentKey aDesc) uname ())
                  (\_student _uname _uid _aDesc score -> Pages.modifyUserScore (scoreKeyOfInfo score) ())
                  pd

      view :: I18N -> Html
      view msg = pageDataAlgebra
                 (\_student _uname _uid _aDesc -> mempty)
                 (\_student _uname _uid _aDesc score -> Bootstrap.rowColMd12 . H.p . B.toMarkup $ formatEvResult toPlainText msg (evaluationOfInfo score))
                 pd

      evaluationInput :: I18N -> Html
      evaluationInput msg = pageDataAlgebra
                        (\_student _uname _uid _aDesc -> evaluationFrame (evConfig as) msg mempty)
                        (\_student _uname _uid _aDesc score ->
                             scoreInfoCata
                             (\_ _ evResult -> evaluationFrameWithDefault msg (evConfig as) evResult mempty)
                             score)
                        pd

viewScorePage :: GETContentHandler
viewScorePage = do
  sk <- getParameter scoreKeyPrm
  sDesc <- userStory $ Story.scoreDesc sk
  setPageContents $ htmlPage (msg_LinkText_ViewUserScore "Score") $
    viewScoreContent sDesc

viewScoreContent :: ScoreDesc -> IHtml
viewScoreContent sd = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 . Bootstrap.table . H.tbody $ do
      (msg . msg_ViewUserScore_Course $ "Course:")   .|. maybe (courseName $ scdCourse sd) (fullGroupName (scdCourse sd)) (scdGroup sd)
      (msg . msg_ViewUserScore_Assessment $ "Assessment:") .|. aTitle
      when (not . T.null $ aDesc) $
        (msg . msg_ViewUserScore_Description $ "Description:") .|. aDesc
    Bootstrap.rowColMd12 . H.p . B.toMarkup . (formatEvResultMaybe toPlainText msg) . fmap evaluationOfInfo . scdScoreInfo $ sd
  where 
    aTitle, aDesc :: Text
    (aTitle, aDesc) = assessment (\title desc _creation _cfg _visible -> (title,desc)) (scdAssessment sd)

evConfig :: Assessment -> EvConfig
evConfig = assessment (\_title _desc _creation cfg _visible -> cfg)

evaluationFrame :: EvConfig -> I18N -> Html -> Html
evaluationFrame evConfig msg content = do
  hiddenInput (fieldName evalConfigParam) (encodeToFay' "inputEvalType" evConfig :: Text)
  withEvConfig evConfig
    (do content
        binaryInput msg Passed        
    )
    (\_ ->
      do content
         percentageInput msg ""
    )
    (do content
        freeFormInput msg ""
    )
  where
    binary = EvCmtResult . binaryResult

evaluationFrameWithDefault :: I18N -> EvConfig -> EvResult -> Html -> Html
evaluationFrameWithDefault msg evConfig evResult content = do
  hiddenInput (fieldName evalConfigParam) (encodeToFay' "inputEvalType" evConfig :: Text)
  withEvResult evResult
    (\binRes ->
        binaryCata (\b -> do
          content
          binaryInput msg b)
        binRes
    )
    (\pctRes -> do
       content
       let Percentage (Scores [p]) = pctRes
       percentageInput msg (show . round $ (100 * p))
    )
    (\freeFormRes -> do
       content
       freeForm (freeFormInput msg) freeFormRes
    )

binaryInput :: I18N -> Result -> Html
binaryInput msg res = do
  Bootstrap.formGroup $ H.div $ Bootstrap.radioButtonGroup (fieldName evaluationResultField) $
            [ (res == Passed, encodeToFay' "inputEvalResult" $ binary Passed, msg $ msg_Evaluation_Accepted "Accepted")
            , (res == Failed, encodeToFay' "inputEvalResult" $ binary Failed, msg $ msg_Evaluation_Rejected "Rejected")
            ]
  where
    binary = EvCmtResult . binaryResult

 -- When the page is dynamic the percentage spinner is hooked on the field
percentageInput :: I18N -> String -> Html
percentageInput msg defaultText = do
  Bootstrap.formGroup . evaluationDiv . Bootstrap.rowColMd12 $ do 
           B.toMarkup . msg $ msg_Evaluation_Percentage "Percentage: "
           H.input ! A.name (B.toValue $ fieldName evaluationPercentagePrm) ! A.type_ "number"
                   ! A.min "0" ! A.max "100"
                   ! A.required ""
                   ! A.value (B.toValue defaultText)
      where
        evaluationDiv :: Html -> Html
        evaluationDiv = H.div ! A.id (B.toValue $ fieldName evaluationPercentageDiv)

freeFormInput :: I18N -> Text -> Html
freeFormInput msg defaultText = do
  Bootstrap.textInputWithDefault (fieldName freeFormEvaluationParam) (msg $ msg_Evaluation_FreeFormEvaluation "Evaluation") defaultText
  H.p . B.toMarkup $ (printf (T.unpack $ msg $ msg_Evaluation_FreeForm_Information $ T.unwords
    [ "Note that this text will be used everywhere as the evaluation itself.  Hence it is recommended to keep"
    , "the length of the text under size %d, otherwise it may not be directly shown." ]) displayableFreeFormResultLength :: String)
             
evalConfigParam = evalConfigParameter (fieldName evaluationConfigField)
freeFormEvaluationParam = stringParameter (fieldName evaluationFreeFormField) "Free format evaluation"

