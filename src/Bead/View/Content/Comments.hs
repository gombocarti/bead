{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Comments (
    CommentOrFeedback
  , submissionDescToCFs
  , submissionDetailsDescToCFs
  , commentsToCFs
  , commentsDiv
  , commentOrFeedbackTime
  , commentOrFeedbackText
  , commentOrFeedbackAuthor
  , sortDecreasingTime
  , feedbacksToCFs
  , forStudentCFs
  ) where

import           Data.List (sortOn)
import           Data.String
import           Data.Map as Map (toList)
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad

import qualified Text.Blaze as B
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Printf

import           Bead.Controller.Pages as Pages
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.Domain.Shared.Evaluation

type CommentOrFeedback = Either (CommentKey, Comment) Feedback

commentOrFeedback
  comment
  feedback
  cf = case cf of
    Left  c -> comment  c
    Right f -> feedback f

commentOrFeedbackTime = commentOrFeedback (commentDate . snd) postDate

commentsToCFs :: [(CommentKey, Comment)] -> [CommentOrFeedback]
commentsToCFs = map Left

feedbacksToCFs :: [Feedback] -> [CommentOrFeedback]
feedbacksToCFs = map Right

-- Converts a given submission description into a list of comments and feedbacks
submissionDescToCFs :: SubmissionDesc -> [CommentOrFeedback]
submissionDescToCFs s = (commentsToCFs . Map.toList $ eComments s) ++ (feedbacksToCFs $ eFeedbacks s)

-- Converts a given submission detailed description into a list of comments and feedbacks
submissionDetailsDescToCFs :: SubmissionDetailsDesc -> [CommentOrFeedback]
submissionDetailsDescToCFs s = (commentsToCFs . Map.toList $ sdComments s) ++ (feedbacksToCFs $ sdFeedbacks s)

-- Sort the items by increasing by the creation time
sortIncreasingTime :: [CommentOrFeedback] -> [CommentOrFeedback]
sortIncreasingTime = sortOn time where
  time = commentOrFeedbackTime

-- Sort the items by descreasing by the creation time
sortDecreasingTime = reverse . sortIncreasingTime

-- Filters out comments and feedback not visible for the students
forStudentCFs :: Bool -> [CommentOrFeedback] -> [CommentOrFeedback]
forStudentCFs ballotBox = filter forStudent where
  forStudent =
    commentOrFeedback
      ((&&) (not ballotBox) . isStudentComment . snd)
      (feedback
         (feedbackInfo
           True
           (const True) -- result
           (const True) -- student
           (const False) -- admin
           (const3 True)) -- evaluated
         p_1_2)

commentsDiv :: String -> UserTimeConverter -> [CommentOrFeedback] -> IHtml
commentsDiv id_ t cs = do
  msg <- getI18N
  return $ do
    seeMore
    Bootstrap.panelGroup ! A.role "tablist" ! Bootstrap.areaMultiselectable "true" $ do
    mapM_ (seeMoreComment msg id_ t) $ zip [1..] (sortDecreasingTime cs)

  where
    seeMore :: Html
    seeMore = --H.script "function seemore(event) { val parent = event.target.parentNode; alert(parent.id); }"
      H.script $ fromString $ concat
        [ "function seemore(toHide, toShow) {"
        , "toHide.forEach(e => document.getElementById(e).remove());"
        , "toShow.forEach(e => document.getElementById(e).style.removeProperty('display'));"
        , "}"
        ]

seeMoreComment :: I18N -> String -> UserTimeConverter -> (Int, CommentOrFeedback) -> Html
seeMoreComment i18n id_ t (n, c) =
  let comment = commentOrFeedbackText i18n c
      badge = T.concat [T.pack . showDate . t $ commentOrFeedbackTime c, " ", commentOrFeedbackAuthor i18n c]
      commentId = fromString $ id_ ++ show n
  in seeMoreComment commentId i18n badge (commentOrFeedbackText i18n c)
  where
    anchorValue :: CommentOrFeedback -> Maybe CommentKey
    anchorValue =
      commentOrFeedback
        (Just . fst)
        (const Nothing)

    maxLength = 100
    maxLines = 5

    seeMoreComment :: String -> I18N -> Text -> Text -> Html
    seeMoreComment id_ i18n badgeText content =
      let heading = do
            maybe mempty (\ac -> H.div ! A.id (anchor ac) $ mempty) (anchorValue c)
            Bootstrap.badge badgeText
      in
        Bootstrap.panel Nothing $ do
          heading
          H.pre ! A.class_ "comment" $ do
            B.toMarkup preview
            when isLargeContent $ do
              H.span ! A.id (fromString dotsId) $ fromString " ..."
              H.span ! A.style "display: none"
                     ! A.id (fromString moreId)
                     $ B.toMarkup rest
          when isLargeContent $
            Bootstrap.buttonOnClick
            ""
            (i18n $ msg_SeeMore_SeeMore "See More")
            (printf "seemore(['%s', '%s'], ['%s'])" dotsId buttonId moreId)
            ! A.id (fromString buttonId)
      where
        (cmtShort, rest) = T.splitAt maxLength content
        preview = if isLargeContent then cmtShort else content
        isLargeContent = (not . T.null) rest || (not . null . drop maxLines . T.lines) content

        dotsId, moreId, buttonId :: String
        dotsId = id_ ++ "-dots"
        moreId = id_ ++ "-more"
        buttonId = id_ ++ "-button"


commentOrFeedbackText :: I18N -> CommentOrFeedback -> Text
commentOrFeedbackText i18n =
  commentOrFeedback
    ((commentCata $ \comment _author _date _type -> comment) . snd)
    (feedback
       (feedbackInfo
         queuedForTest
         (bool testsPassed testsFailed) -- result
         id   -- comment
         id   -- comment
         evaluationText) -- evaluation
       p_1_2)
  where
     queuedForTest = i18n $ msg_Comments_QueuedForTest "The submission is queued for test."

     testsPassed = i18n $ msg_Comments_TestPassed "The submission has passed the tests."
     testsFailed = i18n $ msg_Comments_TestFailed "The submission has failed the tests."

     bool true false x = if x then true else false

     evaluationText result comment _author =
       let text res | T.null comment = res
                    | otherwise      = T.unlines [comment, "", res]
       in withEvResult result
            (\b -> text (translateMessage i18n (binaryResult b)))
            (const $ text (translateMessage i18n (pctResult result)))
            (\(FreeForm msg) -> text msg)

     binaryResult (Binary b) =
      TransMsg $ resultCata (msg_Comments_BinaryResultPassed "The submission is accepted.")
                            (msg_Comments_BinaryResultFailed "The submission is rejected.")
                            b

     pctResult p = TransPrmMsg
      (msg_Comments_PercentageResult "The percentage of the evaluation: %s")
      (maybe "ERROR: Invalid percentage value! Please contact with the administrations"
             doubleToPercentageStr $ percentValue p)
      where
        doubleToPercentageStr = T.pack . show . round . (100 *)

commentOrFeedbackAuthor :: I18N -> CommentOrFeedback -> Text
commentOrFeedbackAuthor i18n =
  commentOrFeedback
    ((commentCata $ \_comment author _date ->
       commentTypeCata
         author -- student
         author -- groupAdmin
         author -- courseAdmin
         author) . snd)-- admin
    (feedback
      (feedbackInfo
        testScript
        (const result) -- result
        (const testScript) -- student
        (const adminTestScript) -- admin
        (\_result _comment author -> author)) -- evaluation
      p_1_2)
  where
    adminTestScript = i18n $ msg_Comments_AuthorTestScript_Private "Test Script (seen by only admins)"
    testScript = i18n $ msg_Comments_AuthorTestScript_Public "Test Script"
    result = testScript
