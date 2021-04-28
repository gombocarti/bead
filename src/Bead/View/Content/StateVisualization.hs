{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bead.View.Content.StateVisualization
  ( formatEvResult
  , formatEvResultMaybe
  , formatSubmissionState
  , Style(..)
  , toBadge
  , toColoredBadge
  , toIcon
  , toLabel
  , toLargeIcon
  , toMediumIcon
  , toPlainText
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.Domain.Shared.Evaluation (EvResult)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content
import           Bead.View.Content.VisualConstants (displayableFreeFormResultLength)
import qualified Bead.Domain.Shared.Evaluation as Eval

data Style a = Style {
    nonEvaluated :: I18N -> a
  , queuedForTest :: I18N -> a
  , testsPassed :: I18N -> a
  , testsFailed :: I18N -> a
  , accepted :: I18N -> a
  , rejected :: I18N -> a
  , percentageTag :: Text -> a
  , freeFormTag :: Text -> Maybe Text -> a
  , freeFormPlaceholder :: Maybe (I18N -> Text)
}

toPlainText :: Style T.Text
toPlainText = Style {
    nonEvaluated = \msg -> msg $ msg_SubmissionState_NonEvaluated "Non-evaluated"
  , queuedForTest = \msg -> msg $ msg_SubmissionState_QueuedForTest "Queued for test"
  , testsPassed = \msg -> msg $ msg_SubmissionState_Tests_Passed "Tests are passed"
  , testsFailed = \msg -> msg $ msg_SubmissionState_Tests_Failed "Tests are failed"
  , accepted = \msg -> msg $ msg_SubmissionState_Accepted "Accepted"
  , rejected = \msg -> msg $ msg_SubmissionState_Rejected "Rejected"
  , percentageTag = id
  , freeFormTag = \text _mTooltip -> text
  , freeFormPlaceholder = Nothing
  }

-- |Convert a 'SubmissionState' into a colored label.
-- This 'Style' is used on the student section of the Home page.
toLabel :: Style H.Html
toLabel = Style {
    nonEvaluated = \msg -> Bootstrap.grayLabel $ nonEvaluated toPlainText msg
  , queuedForTest = \msg -> Bootstrap.grayLabel $ queuedForTest toPlainText msg
  , testsPassed = \msg -> Bootstrap.grayLabel $ testsPassed toPlainText msg
  , testsFailed = \msg -> Bootstrap.grayLabel $ testsFailed toPlainText msg
  , accepted = \msg -> Bootstrap.greenLabel $ accepted toPlainText msg
  , rejected = \msg -> Bootstrap.redLabel $ rejected toPlainText msg
  , percentageTag = Bootstrap.blueLabel
  , freeFormTag = \text mTooltip -> maybe id (\tooltip -> (! A.title (H.toValue tooltip))) mTooltip (Bootstrap.blueLabel text)
  , freeFormPlaceholder = Just $ const "..."
  }

toMediumIcon :: Style H.Html
toMediumIcon = toIcon Bootstrap.Medium

toLargeIcon :: Style H.Html
toLargeIcon = toIcon Bootstrap.Large

-- |Convert a 'SubmissionState' into a colored icon.
-- This 'Style' is used on the admin section of the Home page,
-- at the top of the Evaluation page and on the EvaluationTable page.
toIcon :: Bootstrap.Size -> Style H.Html
toIcon size = Style {
    nonEvaluated = \msg ->
      H.span
          ! A.class_ "glyphicon glyphicon-stop"
          ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
          ! tooltip (nonEvaluated toPlainText msg)
          $ mempty
  , queuedForTest = \msg ->
      H.span
        ! A.class_ "glyphicon glyphicon-wrench"
        ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
        ! tooltip (queuedForTest toPlainText msg)
        $ mempty
  , testsPassed = \msg ->
      H.span
        ! A.class_ "glyphicon glyphicon-ok-circle"
        ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
        ! tooltip (testsPassed toPlainText msg)
        $ mempty
  , testsFailed = \msg ->
      H.span
        ! A.class_ "glyphicon glyphicon-remove-circle"
        ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
        ! tooltip (testsFailed toPlainText msg)
        $ mempty
  , accepted = \msg ->
      H.span
        ! A.class_ "glyphicon glyphicon-thumbs-up"
        ! A.style (H.toValue $ unwords ["color:#00FF00;", iconSize])
        ! tooltip (accepted toPlainText msg)
        $ mempty
  , rejected = \msg ->
      H.span
        ! A.class_ "glyphicon glyphicon-thumbs-down"
        ! A.style (H.toValue $ unwords ["color:#FF0000;", iconSize])
        ! tooltip (rejected toPlainText msg)
        $ mempty
  , percentageTag =
      Bootstrap.blueLabel
  , freeFormTag =
      \text mTooltip -> maybe id (\tooltip -> (! A.title (H.toValue tooltip))) mTooltip (Bootstrap.blueLabel text)
  , freeFormPlaceholder =
      Just $ const "..."
  }
  where
    tooltip :: T.Text -> H.Attribute
    tooltip s = A.title (H.toValue s)

    iconSize :: String
    iconSize = unwords ["font-size:", Bootstrap.sizeCata "medium" "large" "xx-large" size]

-- |This style is used in list groups that lists submissions.
toBadge :: Style H.Html
toBadge = Style {
    nonEvaluated = \msg -> Bootstrap.badge $ nonEvaluated toPlainText msg
  , queuedForTest = \msg -> Bootstrap.badge $ queuedForTest toPlainText msg
  , testsPassed = \msg -> Bootstrap.badge $ testsPassed toPlainText msg
  , testsFailed = \msg -> Bootstrap.badge $ testsFailed toPlainText msg
  , accepted = \msg -> Bootstrap.badge $ accepted toPlainText msg
  , rejected = \msg -> Bootstrap.badge $ rejected toPlainText msg
  , percentageTag = Bootstrap.badge
  , freeFormTag = \text mTooltip -> maybe id (\tooltip -> (! A.title (H.toValue tooltip))) mTooltip (Bootstrap.badge text)
  , freeFormPlaceholder = Just $ \msg -> msg $ msg_SubmissionState_FreeFormEvaluated "Evaluated"
  }

-- |Convert a 'SubmissionState' into a colored icon.
-- Uses the same color as labels. This is forward-compatible with Bootstrap 4,
-- which uses same colors for pill badges and labels.
-- When Bootstrap 4 rolls in, colored badges can use 'Bootstrap.Alert'.
toColoredBadge :: Style H.Html
toColoredBadge = toBadge {
    accepted = \msg -> (accepted toBadge msg) ! acceptedColor
  , rejected = \msg -> (rejected toBadge msg) ! rejectedColor
  , percentageTag = \text -> (percentageTag toBadge text) ! evaluatedColor
  , freeFormTag = \text mTooltip -> (freeFormTag toBadge text mTooltip) ! evaluatedColor
  }

acceptedColor :: H.Attribute
acceptedColor = A.style "background-color: #5cb85c;"

rejectedColor :: H.Attribute
rejectedColor = A.style "background-color: #d9534f;"

evaluatedColor :: H.Attribute
evaluatedColor = A.style "background-color: #337ab7;"

formatSubmissionState :: forall a . Style a -> I18N -> SubmissionState -> a
formatSubmissionState style msg =
  submissionStateCata
    (nonEvaluated style msg)
    (queuedForTest style msg)
    tested
    (\_key result -> formatEvResult style msg result) -- evaluated

  where
    tested :: Bool -> a
    tested = bool (testsPassed style msg) (testsFailed style msg)

formatEvResultMaybe :: Style a -> I18N -> Maybe Eval.EvResult -> a
formatEvResultMaybe style msg Nothing = nonEvaluated style msg
formatEvResultMaybe style msg (Just eval) = formatEvResult style msg eval

formatEvResult :: forall a . Style a -> I18N -> Eval.EvResult -> a
formatEvResult style msg =
  Eval.evResultCata
    (Eval.binaryCata (Eval.resultCata (accepted style msg) (rejected style msg)))
    percentage
    (Eval.freeForm $ \text ->
        case freeFormPlaceholder style of
          Just placeHolder ->
            let cell = if T.length text < displayableFreeFormResultLength then text else placeHolder msg in
              freeFormTag style cell (Just text)
          Nothing ->
            freeFormTag style text Nothing)

  where
    percentage :: Eval.Percentage -> a
    percentage (Eval.Percentage (Eval.Scores [p])) = percentageTag style $ T.concat [T.pack . show . round $ (100 * p), "%"]
    percentage _ = percentageTag style "???%"
