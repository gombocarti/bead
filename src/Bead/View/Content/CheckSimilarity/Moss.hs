{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.CheckSimilarity.Moss (
    checkSimilarityMoss
  , viewMossReport
  ) where

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.View.BeadContext (getMossScriptPath)
import           Bead.View.Content as C
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.ContentHandler (modifyPageSettings)
import           Bead.View.DataBridge as D (Parameter, name)

import           Data.Function (on)
import           Data.Bifunctor (second)
import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Exit (ExitCode(ExitSuccess, ExitFailure))
import qualified Text.Blaze.Html5 as H

checkSimilarityMoss :: ViewModifyHandler
checkSimilarityMoss = ViewModifyHandler checkSimilarityPage postCheckSimilarity

viewMossReport :: ViewHandler
viewMossReport = ViewHandler viewMossReportPage

programmingLanguage :: Parameter ProgrammingLanguage
programmingLanguage = programmingLanguagePrm "programming-language"

checkSimilarityPage :: GETContentHandler
checkSimilarityPage = do
  ak <- getParameter assignmentKeyPrm
  userStory $ Story.isAdministratedAssignment ak
  setPageContents $ htmlPage (Pages.pageValue $ Pages.similarityCheckMossWithText ak) $ similarityCheckContents ak

postCheckSimilarity :: POSTContentHandler
postCheckSimilarity = do
  ak <- getParameter assignmentKeyPrm
  prLang <- getParameter programmingLanguage
  moss <- beadHandler $ getMossScriptPath
  return $ Action $ do
    invocationKey <- Story.checkSimilarityMoss moss prLang ak
    return $ redirection $ Pages.viewMossScriptOutput invocationKey ()

viewMossReportPage :: GETContentHandler
viewMossReportPage = do
  invocationKey <- getParameter mossScriptInvocationKeyPrm
  invocationResult <- userStory $ Story.loadMossScriptInvocationResult invocationKey
  let (httpEquiv, contents) = viewMossReportContents invocationResult
  maybe (return ()) (modifyPageSettings . setHttpEquiv) httpEquiv
  setPageContents $ htmlPage (Pages.pageValue $ Pages.viewMossScriptOutputWithText invocationKey) contents

similarityCheckContents :: AssignmentKey -> IHtml
similarityCheckContents ak = do
  msg <- getI18N
  return $ do
    Bootstrap.alert Bootstrap.Info $ do
      H.text . msg $ msg_Moss_Info "We use Moss, an external tool for checking similarity of submissions. You can learn more about Moss at the its webpage at"
      H.text " "
      Bootstrap.linkNewTab mossUrl mossUrl
    postForm (routeOf $ Pages.similarityCheckMoss ak ()) $ do
      Bootstrap.labelFor (D.name programmingLanguage) (msg $ msg_Moss_ProgrammingLanguage "Programming Language of Submissions")
      Bootstrap.selectionWithPlaceholder (D.name programmingLanguage) (msg $ msg_Moss_ChooseProgrammingLanguage "Choose a Programming Language") programmingLanguages
      Bootstrap.submitButtonColorful T.empty (msg $ msg_Moss_Submit "Send Submissions for Similarity Check")

    where
      programmingLanguages :: [(ProgrammingLanguage, Text)]
      programmingLanguages = sortOn snd $ map (\lang -> (lang, prHumanReadable lang)) C.programmingLanguages

      mossUrl :: Text
      mossUrl = "http://theory.stanford.edu/~aiken/moss/"

viewMossReportContents :: Maybe MossScriptInvocation -> (Maybe HttpEquiv, IHtml)
viewMossReportContents result = do
  second (Bootstrap.rowColMd12 <$>) $
    case result of
      Nothing -> (Just (Reload secondsBetweenReloads), notYetFinished)
      Just invocationResult ->
        (Nothing, mossScriptInvocationCata completed unsuccessful notInterpretable invocationResult)

  where
    notYetFinished :: IHtml
    notYetFinished = do
      msg <- getI18N
      return $
        Bootstrap.alert Bootstrap.Info $ do
          H.text . msg $ msg_Moss_ReportIsNotYetReady "Your report is not yet done."
          H.text " "
          H.text . msg $ msg_Moss_WeAreRefreshingThePage "We are refreshing the page"
          H.text " "
          H.b . H.text $ translateMessage msg $ TransPrmMsg (msg_Moss_EveryNSeconds "every %s seconds") (T.pack . show $ secondsBetweenReloads)
          H.text "."
          H.text . msg $ msg_Moss_PleaseBePatient "We kindly ask your patience until the report is done. It should not take long."

    secondsBetweenReloads :: Int
    secondsBetweenReloads = 5

    completed :: Text -> Text -> IHtml
    completed _output reportUrl = do
      msg <- getI18N
      return $ do
        Bootstrap.panel
          (Just Bootstrap.Success)
          (Just . H.text . msg $ msg_Moss_ReportIsDone "Your report is done and available at the following link.")
          (Bootstrap.linkNewTab reportUrl reportUrl)

    unsuccessful :: Text -> ExitCode -> IHtml
    unsuccessful output _exitCode = errorHappened output

    notInterpretable :: Text -> IHtml
    notInterpretable = errorHappened

    errorHappened :: Text -> IHtml
    errorHappened output = do
      msg <- getI18N
      return $
        Bootstrap.panel
          (Just Bootstrap.Danger)
          (Just . H.text . msg $ msg_Moss_ErrorDuringSending "An error happened during sending submissions for similarity check. The following log is generated during the process.")
          (H.pre $ H.toMarkup output)

