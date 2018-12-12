{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bead.View.Content.SubmissionState
  ( formatSubmissionState
  , Style(..)
  , toBadge
  , toColoredBadge
  , toIcon
  , toLabel
  , toLargeIcon
  , toMediumIcon
  , toPlainText
  ) where

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
  , testsPassed :: I18N -> a
  , testsFailed :: I18N -> a
  , accepted :: I18N -> a
  , rejected :: I18N -> a
  , percentageTag :: String -> a
  , freeFormTag :: String -> Maybe String -> a
  , freeFormPlaceholder :: Maybe (I18N -> String)
}

toPlainText :: Style T.Text
toPlainText = Style {
    nonEvaluated = \msg -> T.pack $ msg $ msg_SubmissionState_NonEvaluated "Non-evaluated"
  , testsPassed = \msg -> T.pack $ msg $ msg_SubmissionState_Tests_Passed "Tests are passed"
  , testsFailed = \msg -> T.pack $ msg $ msg_SubmissionState_Tests_Failed "Tests are failed"
  , accepted = \msg -> T.pack $ msg $ msg_SubmissionState_Accepted "Accepted"
  , rejected = \msg -> T.pack $ msg $ msg_SubmissionState_Rejected "Rejected"
  , percentageTag = T.pack
  , freeFormTag = \text _mTooltip -> T.pack text
  , freeFormPlaceholder = Nothing
  }

-- |Convert a 'SubmissionState' into a colored label.
-- This 'Style' is used on the student section of the Home page.
toLabel :: Style H.Html
toLabel = Style {
    nonEvaluated = \msg -> Bootstrap.grayLabel $ msg $ msg_SubmissionState_NonEvaluated "Non-evaluated"
  , testsPassed = \msg -> Bootstrap.grayLabel $ msg $ msg_SubmissionState_Tests_Passed "Tests are passed"
  , testsFailed = \msg -> Bootstrap.grayLabel $ msg $ msg_SubmissionState_Tests_Failed "Tests are failed"
  , accepted = \msg -> Bootstrap.greenLabel $ msg $ msg_SubmissionState_Accepted "Accepted"
  , rejected = \msg -> Bootstrap.redLabel $ msg $ msg_SubmissionState_Rejected "Rejected"
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
-- at the top of the Evaluation page, and on the EvaluationTable page.
toIcon :: Bootstrap.Size -> Style H.Html
toIcon size = Style {
    nonEvaluated = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-stop"
          ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_NonEvaluated "Non evaluated")
          $ mempty
  , testsPassed = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-ok-circle"
          ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_Tests_Passed "Tests are passed")
          $ mempty
  , testsFailed = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-remove-circle"
          ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_Tests_Failed "Tests are failed")
          $ mempty
  , accepted = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-thumbs-up"
          ! A.style (H.toValue $ unwords ["color:#00FF00;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_Accepted "Accepted")
          $ mempty
  , rejected = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-thumbs-down"
          ! A.style (H.toValue $ unwords ["color:#FF0000;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_Rejected "Rejected")
          $ mempty
  , percentageTag =
      Bootstrap.blueLabel
  , freeFormTag =
      \text mTooltip -> maybe id (\tooltip -> (! A.title (H.toValue tooltip))) mTooltip (Bootstrap.blueLabel text)
  , freeFormPlaceholder =
      Just $ const "..."
  }
  where
    tooltip :: String -> H.Attribute
    tooltip s = A.title (H.toValue s)

    iconSize :: String
    iconSize = unwords ["font-size:", Bootstrap.sizeCata "medium" "large" "xx-large" size]

-- |This style is used in list groups that lists submissions.
toBadge :: Style H.Html
toBadge = Style {
    nonEvaluated = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_NonEvaluated "Non-evaluated"
  , testsPassed = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_Tests_Passed "Tests are passed"
  , testsFailed = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_Tests_Failed "Tests are failed"
  , accepted = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_Accepted "Accepted"
  , rejected = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_Rejected "Rejected"
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

formatSubmissionState :: forall a. Style a -> I18N -> SubmissionState -> a
formatSubmissionState style msg =
  submissionStateCata
    (nonEvaluated style msg)
    tested
    (\_key result -> val result) -- evaluated

  where
    tested :: Bool -> a
    tested = bool (testsPassed style msg) (testsFailed style msg)

    val :: Eval.EvResult -> a
    val = Eval.evResultCata
            (Eval.binaryCata (Eval.resultCata (accepted style msg) (rejected style msg)))
            percentage
            (Eval.freeForm $ \text ->
              case freeFormPlaceholder style of
                Just placeHolder ->
                  let cell = if length text < displayableFreeFormResultLength then text else placeHolder msg in
                    freeFormTag style cell (Just text)
                Nothing ->
                  freeFormTag style text Nothing)

      where
        percentage :: Eval.Percentage -> a
        percentage (Eval.Percentage (Eval.Scores [p])) = percentageTag style $ concat [show . round $ (100 * p), "%"]
        percentage _ = percentageTag style "???%"

evResult :: forall a. a -> a -> (String -> a) -> (String -> a) -> Eval.EvResult -> a
evResult passed failed percentage freeFormMsg =
 Eval.evResultCata
  (Eval.binaryCata (Eval.resultCata passed failed))
  score
  (Eval.freeForm freeFormMsg)
  where
    percent :: Double -> String
    percent x = concat [show . round $ (100 * x), "%"]

    score :: Eval.Percentage -> a
    score (Eval.Percentage (Eval.Scores [p])) = percentage $ percent p
    score _                                   = percentage "?%"
